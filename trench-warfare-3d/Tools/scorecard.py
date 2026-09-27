#!/usr/bin/env python3
"""Numbers for how navigable and maintainable the repo is, so a change that makes it worse is caught.

    python Tools/scorecard.py                         print the scores
    python Tools/scorecard.py --selftest              also run Tools/selftest.py (about a minute)
    python Tools/scorecard.py --history FILE          append this run to FILE (JSON lines) and compare each metric
                                                      with its value in the last clean run in FILE: every metric
                                                      that got worse, or could not be measured, is REGRESSED and
                                                      the exit code is 1
    python Tools/scorecard.py --history FILE --accept  the same, but record this run as the new clean baseline
                                                      (a regression you meant; say why in the commit)

A run with a regression is recorded but never becomes the baseline, so the regression is reported again on every
run until it is fixed or accepted.

No Unity needed. Test counts come from the last gate's test-results-<mode>.xml, with their age, so a stale number
is visible. Each metric says which way is better; "info" metrics are recorded but never flagged.
"""
import json
import re
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
REPO = ROOT.parent
PROJ = ROOT / 'Assets' / '_Project'
REF = REPO / 'docs' / 'reference'

# metric -> 'lower' / 'higher' is better, or 'info'
DIRECTION = {
    'validate_ok': 'higher', 'codemap_errors': 'lower', 'codemap_seconds': 'info',
    'mandatory_read_tokens': 'lower', 'claude_md_lines': 'info', 'tasks_md_tokens': 'info', 'workflow_md_tokens': 'info',
    'unrouted_files': 'lower', 'pending_until_tags': 'info', 'inbox_notes': 'info',
    'cs_files': 'info', 'cs_lines': 'info', 'files_over_1000_lines': 'lower', 'largest_file_lines': 'lower',
    'scenehooks_refs': 'lower', 'scenehooks_files': 'lower', 'statics_explained': 'lower', 'stub_systems': 'info',
    'selftest_failed': 'lower', 'selftest_cases': 'higher',
    'editmode_tests': 'higher', 'editmode_failed': 'lower', 'playmode_tests': 'higher', 'playmode_failed': 'lower',
}
OPTIONAL = {'selftest_failed', 'selftest_cases'}   # measured only with --selftest: absent is not a regression


def read(p):
    return p.read_text(encoding='utf-8-sig', errors='replace')


def run(cmd, cwd=ROOT, timeout=600):
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, timeout=timeout)
    return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')


def scores(with_selftest):
    s = {}
    t0 = time.time()
    code, out = run([sys.executable, 'Tools/codemap.py', '--check'])
    s['codemap_seconds'] = round(time.time() - t0, 1)
    s['codemap_errors'] = sum(1 for l in out.split('\n') if l.startswith('codemap: '))
    s['unrouted_files'] = out.count('is named on no agent page')
    s['validate_ok'] = int(run([sys.executable, 'validate.py'])[0] == 0)

    claude = read(REPO / 'CLAUDE.md')
    workflow = read(REF / 'workflow.md')
    everyday = [x for x in re.split(r'(?m)^## ', workflow) if re.match(r'(1|2|3|4|5|8)\. ', x)]
    s['mandatory_read_tokens'] = (len(claude) + sum(len(x) for x in everyday)) // 4
    s['claude_md_lines'] = len(claude.rstrip('\n').split('\n'))
    s['tasks_md_tokens'] = len(read(REF / 'tasks.md')) // 4
    s['workflow_md_tokens'] = len(workflow) // 4
    s['pending_until_tags'] = sum(len(re.findall(r'\(until\s+"', read(p))) for p in [REPO / 'CLAUDE.md', *REF.glob('*.md')])
    s['inbox_notes'] = len([p for p in (REPO / 'docs' / 'inbox').glob('*.md') if p.name != 'README.md'])

    files = [p for p in PROJ.rglob('*.cs')]
    lines = {p: len(read(p).split('\n')) for p in files}
    prod = [p for p in files if 'Tests' not in p.parts]
    s['cs_files'] = len(files)
    s['cs_lines'] = sum(lines.values())
    big = sorted((p for p in prod if lines[p] > 1000), key=lambda p: -lines[p])
    s['files_over_1000_lines'] = len(big)
    s['largest_file_lines'] = max(lines[p] for p in prod)
    s['largest_files'] = [f'{p.relative_to(PROJ).as_posix()} {lines[p]}' for p in big]
    refs = {p: len(re.findall(r'\bSceneHooks\.', read(p))) for p in prod}
    s['scenehooks_refs'] = sum(refs.values())
    s['scenehooks_files'] = sum(1 for v in refs.values() if v)
    lifecycle = PROJ / 'Tests' / 'EditMode' / 'StaticLifecycleTests.cs'
    s['statics_explained'] = len(re.findall(r'^\s*\["\w+"\]\s*=', read(lifecycle), re.M)) if lifecycle.exists() else -1
    s['stub_systems'] = sum(1 for p in prod if 'NotImplementedException' in read(p))

    for mode in ('EditMode', 'PlayMode'):
        x = ROOT / f'test-results-{mode}.xml'
        key = mode.lower()
        if x.exists():
            head = read(x)[:2000]
            total = re.search(r'\btotal="(\d+)"', head)
            failed = re.search(r'\bfailed="(\d+)"', head)
            s[f'{key}_tests'] = int(total.group(1)) if total else -1
            s[f'{key}_failed'] = int(failed.group(1)) if failed else -1
            s[f'{key}_results_age_hours'] = round((time.time() - x.stat().st_mtime) / 3600, 1)

    if with_selftest:
        code, out = run([sys.executable, 'Tools/selftest.py'], timeout=900)
        m = re.search(r'(\d+) of (\d+) cases behaved', out)
        if m:
            s['selftest_cases'] = int(m.group(2))
            s['selftest_failed'] = int(m.group(2)) - int(m.group(1))
    s['head'] = run(['git', 'rev-parse', '--short', 'HEAD'], cwd=REPO)[1].strip()
    s['time'] = time.strftime('%Y-%m-%d %H:%M')
    return s


def compare(rows, s):
    '''(regressed, improved) lines for this run's scores s against rows, the history so far. Each metric is compared
    with its value in the last row that has no regression (or was accepted). A metric that baseline had and this run
    lacks, or reads -1 (its source was unreadable), is a regression: an unmeasured number must not pass.'''
    clean = [r for r in rows if not r.get('regressed') or r.get('accepted')]
    worse, better = [], []
    for k, d in DIRECTION.items():
        if d == 'info':
            continue
        base = next((r[k] for r in reversed(clean) if r.get(k, -1) != -1), None)
        if base is None:
            continue
        now = s.get(k)
        if now is None and k in OPTIONAL:
            continue
        if now is None or now == -1:
            worse.append(f'REGRESSED {k}: {base} -> {"not measured" if now is None else -1}')
        elif (d == 'lower' and now > base) or (d == 'higher' and now < base):
            worse.append(f'REGRESSED {k}: {base} -> {now}')
        elif now != base:
            better.append(f'improved  {k}: {base} -> {now}')
    return worse, better


def main():
    s = scores('--selftest' in sys.argv)
    for k, v in s.items():
        if k != 'largest_files':
            print(f'{k:28} {v}')
    for f in s['largest_files']:
        print(f'{"":28} {f}')
    if '--history' not in sys.argv:
        return
    hist = Path(sys.argv[sys.argv.index('--history') + 1])
    rows = []
    if hist.exists():
        for l in hist.read_text(encoding='utf-8').split('\n'):
            try:
                rows.append(json.loads(l))
            except ValueError:
                pass   # a blank or half-written line
    worse, better = compare(rows, s)
    s['regressed'] = [w.split()[1].rstrip(':') for w in worse]
    if worse and '--accept' in sys.argv:
        s['accepted'] = True
    with hist.open('a', encoding='utf-8') as f:
        f.write(json.dumps(s) + '\n')
    if not rows:
        print('\nfirst run in this history: nothing to compare')
        return
    for line in better + worse:
        print(line)
    note = ' (accepted: this run is the new baseline)' if worse and s.get('accepted') else ''
    print(f'\n{len(worse)} regressions against the last clean run{note}')
    sys.exit(1 if worse and not s.get('accepted') else 0)


if __name__ == '__main__':
    main()
