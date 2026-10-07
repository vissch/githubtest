#!/usr/bin/env python3
"""Did these commits fix what they say? The check behind a review-fix unit, by script.

    python Tools/review/fixcheck.py --tree <a checkout that is fixcheck's own> --base <commit> --head <commit>
                                    --lane lane/sim/x --ids U1 U4 [--unit rv-02-fleet-fires] [--out FILE]
                                    [--unity <Unity.exe>] [--no-run]

WHY. A fix unit on the relay passes when every finding's id stands in square brackets in a commit message. That says
the leg wrote the id, not that the bug is gone: of the first three units, two had a fix that did not hold and one
commit added a UTF-8 BOM to nine files, all found by a second reader and none by a script (2026-10-06). This is the
script. For the commits base..head on a lane it answers four things:

  1. Every id is named in a commit message, and has a test tagged with it (a comment or a title that holds `[ID]`)
     or a sentence `[ID] no test: why` in a commit message.
  2. Each tagged test is RED on the old code and GREEN on the fix. The old code is the tree at head with every
     file that is not a test put back to base, so the new tests run against the code as it was. A test that is
     green there cannot fail and proves nothing. Red there is PROVED only when the failure names the id (an
     assert message that holds `[ID]`, a self-test case titled with it); red with a failure that does not name it
     is RED: a unit fixes several things at once, and a test can be red on the old code for another fix's reason. A follow-up that only makes an earlier fix's test able to fail
     names that fix (`[ID] fix in base: <commit>` in a commit message, or --fix-in-base ID=COMMIT): the files
     that commit changed are put back too. A test that is red there only because the old code does not
     compile with it is said so (`weak`): it shows the fix added a name, not that the bug was real.
  3. No file gained a UTF-8 BOM.
  4. No file is outside the lane: a sim lane keeps to Sim/, Net/, Data/ and the sim tests, a show lane keeps out
     of them, a tools lane changes no game code (CLAUDE.md, Lanes).

What it cannot see: a fix that is wrong where no test looks. That stays the reviewer's.

It runs python tests itself (a `test_*.py` as `python <file> Class.test`, Tools/selftest.py whole) and Unity tests
through Unity in batch mode by test name (EditMode, and PlayMode for a test under Tests/PlayMode). Without a Unity
it says UNCHECKED for those, never PASS. --tree must be a checkout nobody else uses, clean, with "fixcheck" in its
folder name: the script moves its HEAD and resets it. The verdict is written as JSON (--out; default: the Drive's
review-fixes/checked/<unit>.json when that folder is there) and printed. Exit 0 PASS, 1 FAIL, 2 UNCHECKED.
Stdlib only.
"""
import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

DRIVE = Path('G:/My Drive/TW3D-pipeline/review-fixes/checked')
UNITY = Path('C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe')
CODE = 'trench-warfare-3d/Assets/_Project/'
SIM = tuple(CODE + d for d in ('Sim/', 'Net/', 'Data/', 'Tests/Sim/'))
BOM = b'\xef\xbb\xbf'
TEST_ATTR = re.compile(r'^\s*\[(Test|UnityTest|TestCase|TestCaseSource)\b')
NOTES = []          # why a tagged test did not run: the last lines its runner printed, kept in the record
SAID = {}           # what a red test said when it failed (its message), by test: filled by the runners
CS_METHOD = re.compile(r'^\s*public\s+(?:static\s+)?[\w<>\[\], .]+?\s+(\w+)\s*\(')


def git(args, cwd, check=True, text=True):
    p = subprocess.run(['git'] + args, cwd=cwd, capture_output=True)
    if check and p.returncode:
        raise SystemExit('fixcheck: git %s failed: %s' % (' '.join(args[:3]), p.stderr.decode('utf-8', 'replace').strip()))
    return p.stdout.decode('utf-8', 'replace') if text else p.stdout


def is_test(path):
    name = path.rsplit('/', 1)[-1]
    return (path.startswith(CODE + 'Tests/') or '/Tests/' in path and path.endswith('.cs')
            or name.startswith('test_') and name.endswith('.py') or name == 'selftest.py')


def kind_of(lane):
    return 'sim' if lane.startswith('lane/sim/') else 'tools' if lane.rstrip('/').endswith('tools') else 'show'


def outside_lane(kind, paths):
    """Files the lane may not change. Docs go with any lane; a tools lane carries no game code at all."""
    game = [p for p in paths if p.startswith('trench-warfare-3d/Assets/')]
    if kind == 'sim':
        return [p for p in game if not p.startswith(SIM)]
    if kind == 'tools':
        return game
    return [p for p in game if p.startswith(SIM)]


def changed(tree, base, head):
    out = git(['diff', '--no-renames', '--name-status', '-z', base, head], tree).split('\0')
    return [(out[i], out[i + 1]) for i in range(0, len(out) - 1, 2)]


def gained_bom(tree, base, head, files):
    out = []
    for status, path in files:
        if status == 'D':
            continue
        new = git(['show', '%s:%s' % (head, path)], tree, text=False)[:3]
        old = b'' if status == 'A' else git(['show', '%s:%s' % (base, path)], tree, check=False, text=False)[:3]
        if new == BOM and old != BOM:
            out.append(path)
    return out


def find_tests(path, text, ids):
    """{id: [test]} for the ids tagged in one test file. A tag is `[ID]` in a comment or a title. In a .cs or a
    test_*.py file the test is the one whose header the tag sits in, else the one whose body holds it."""
    lines, found = text.split('\n'), {}
    name = path.rsplit('/', 1)[-1]
    for n, line in enumerate(lines):
        here = [i for i in ids if '[%s]' % i in line]
        if not here:
            continue
        if name == 'selftest.py':
            m = re.search(r"case\(\s*(['\"])(.*?)\1", line)
            test = {'runner': 'selftest', 'file': path, 'name': m.group(2)} if m else None
        elif name.endswith('.py'):
            test = py_test(path, lines, n)
        else:
            test = cs_test(path, lines, n)
        for i in here:
            if test and test not in found.setdefault(i, []):
                found[i].append(test)
            found.setdefault(i, [])
    return found


def py_test(path, lines, n):
    def is_def(s):
        return re.match(r'^\s+def (test_\w+)\(', s)
    after = next((l for l in lines[n + 1:n + 6] if l.strip() and not l.strip().startswith('#')), '')
    m = is_def(after) if lines[n].strip().startswith('#') and is_def(after) and not _inside(lines, n) else None
    if not m:
        m = next((is_def(l) for l in reversed(lines[:n + 1]) if is_def(l)), None)
    cls = next((re.match(r'^class (\w+)', l) for l in reversed(lines[:n + 1]) if re.match(r'^class (\w+)', l)), None)
    return {'runner': 'python', 'file': path, 'name': '%s.%s' % (cls.group(1), m.group(1))} if m and cls else None


def _inside(lines, n):
    """Is a comment line inside a function body (indented deeper than the def above it)?"""
    indent = len(lines[n]) - len(lines[n].lstrip())
    for l in reversed(lines[:n]):
        if re.match(r'^\s*def \w+\(', l):
            return indent > len(l) - len(l.lstrip())
        if re.match(r'^class \w+', l):
            return False
    return False


def cs_test(path, lines, n):
    starts = []                                     # (line of the first attribute, method name)
    i = 0
    while i < len(lines):
        if TEST_ATTR.match(lines[i]):
            j = i
            while j < len(lines) and not CS_METHOD.match(lines[j]):
                j += 1
            if j < len(lines):
                starts.append((i, CS_METHOD.match(lines[j]).group(1)))
            i = j
        i += 1
    if not starts:
        return None
    k = n + 1                                       # a tag in the comment above a test belongs to that test
    while k < len(lines) and (not lines[k].strip() or lines[k].strip().startswith('//')):
        k += 1
    method = next((m for s, m in starts if s == k), None) if lines[n].strip().startswith('//') else None
    if not method:
        method = next((m for s, m in reversed(starts) if s <= n), starts[0][1])
    ns = next((re.match(r'^\s*namespace ([\w.]+)', l) for l in lines if re.match(r'^\s*namespace ([\w.]+)', l)), None)
    cls = next((re.search(r'\bclass (\w+)', l) for l in reversed(lines[:n + 1]) if re.search(r'\bclass (\w+)', l)), None)
    if not cls:
        return None
    full = '.'.join(x for x in (ns.group(1) if ns else '', cls.group(1), method) if x)
    return {'runner': 'unity', 'file': path, 'name': full,
            'platform': 'PlayMode' if '/Tests/PlayMode/' in path else 'EditMode'}


# ----- running the tests -----
def project_of(tree):
    return tree / 'trench-warfare-3d' if (tree / 'trench-warfare-3d').is_dir() else tree


def run_python(tree, tests):
    out, proj = {}, project_of(tree)
    for t in tests:
        rel = Path(t['file']).relative_to('trench-warfare-3d').as_posix() if t['file'].startswith('trench-warfare-3d/') else t['file']
        p = subprocess.run([sys.executable, rel, t['name']], cwd=proj, capture_output=True,
                           env=dict(os.environ, PYTHONDONTWRITEBYTECODE='1'))
        said = (p.stdout + p.stderr).decode('utf-8', 'replace')
        ran = re.search(r'^Ran (\d+) test', said, re.M)
        out[key(t)] = 'missing' if not ran or ran.group(1) == '0' else 'green' if p.returncode == 0 else 'red'
        if out[key(t)] == 'red':
            SAID[key(t)] = ' '.join(said.split())[-500:]
    return out


def run_selftest(tree, tests):
    proj = project_of(tree)
    p = subprocess.run([sys.executable, 'Tools/selftest.py'], cwd=proj, capture_output=True,
                       env=dict(os.environ, PYTHONDONTWRITEBYTECODE='1'))
    said = (p.stdout + p.stderr).decode('utf-8', 'replace')
    out = {}
    for t in tests:
        m = re.search(r'^(ok|FAIL)\s+%s' % re.escape(t['name']), said, re.M)
        out[key(t)] = 'missing' if not m else 'green' if m.group(1) == 'ok' else 'red'
        if out[key(t)] == 'red':
            SAID[key(t)] = 'FAIL  ' + t['name']
    if 'missing' in out.values():
        NOTES.append('Tools/selftest.py did not print every tagged case (exit %d). Its last lines: %s'
                     % (p.returncode, ' / '.join(l.strip() for l in said.strip().splitlines()[-6:])[-700:]))
    return out


def unity_cmd(unity):
    return [sys.executable, str(unity)] if str(unity).endswith('.py') else [str(unity)]


def run_unity(tree, tests, unity):
    out = {}
    for platform in sorted({t['platform'] for t in tests}):
        batch = [t for t in tests if t['platform'] == platform]
        with tempfile.TemporaryDirectory(prefix='fixcheck-') as tmp:
            xml, log = Path(tmp) / 'results.xml', Path(tmp) / 'unity.log'
            cmd = unity_cmd(unity) + ['-batchmode', '-runTests', '-projectPath', str(project_of(tree)),
                                      '-testPlatform', platform, '-testFilter', ';'.join(t['name'] for t in batch),
                                      '-testResults', str(xml), '-logFile', str(log)]
            subprocess.run(cmd + (['-nographics'] if platform == 'PlayMode' else []), capture_output=True)
            seen, why = {}, {}
            if xml.exists():
                for c in ET.parse(xml).getroot().iter('test-case'):
                    seen[c.get('fullname', '')] = c.get('result', '')
                    why[c.get('fullname', '')] = ' '.join((c.findtext('failure/message') or '').split())[:500]
            said = log.read_text(encoding='utf-8', errors='replace') if log.exists() else ''
            broken = not seen and re.search(r'error CS\d+', said)
            if broken:
                NOTES.append('Unity %s did not compile: %s' % (platform, ' / '.join(
                    l.strip() for l in said.splitlines() if re.search(r'error CS\d+', l))[:700]))
            for t in batch:
                got = [r for n, r in seen.items() if n == t['name'] or n.startswith(t['name'] + '(')]
                out[key(t)] = ('compile' if broken else 'missing' if not got
                               else 'green' if all(r == 'Passed' for r in got) else 'red')
                if out[key(t)] == 'red':
                    SAID[key(t)] = ' / '.join(w for n, w in why.items()
                                              if w and (n == t['name'] or n.startswith(t['name'] + '(')))[:500]
    return out


def key(t):
    return '%s::%s' % (t['file'], t['name'])


def run_all(tree, tests, unity):
    out = {}
    out.update(run_python(tree, [t for t in tests if t['runner'] == 'python']))
    if any(t['runner'] == 'selftest' for t in tests):
        out.update(run_selftest(tree, [t for t in tests if t['runner'] == 'selftest']))
    engine = [t for t in tests if t['runner'] == 'unity']
    if engine and unity:
        out.update(run_unity(tree, engine, unity))
    else:
        out.update({key(t): 'no unity' for t in engine})
    return out


def old_code(tree, base, head, files, earlier=None):
    """Put every changed file that is not a test back to base, as a commit of its own that is never pushed: a tool
    that reads HEAD (selftest.py builds its copy from it) then sees the old code too. Made with plumbing, so no
    hook runs on it. earlier (a commit already in the base): the fix these tests are for was made there, so the
    files that commit changed go back to what they were before it as well."""
    back = [(st, path, base) for st, path in files]
    if earlier:
        back += [(st, path, earlier + '^') for st, path in changed(tree, earlier + '^', earlier)]
    for status, path, to in back:
        if is_test(path):
            continue
        if status == 'A':
            git(['rm', '-q', '-f', '--ignore-unmatch', '--', path], tree)
        else:
            git(['checkout', '-q', to, '--', path], tree)
    git(['add', '-A'], tree)
    commit = git(['commit-tree', git(['write-tree'], tree).strip(), '-p', head, '-m',
                  'fixcheck: the new tests on the old code'], tree).strip()
    git(['update-ref', '--no-deref', 'HEAD', commit], tree)


def verdict_of(i, tests, old, new, named, why, old_said=None):
    if not named:
        return 'FAIL', 'no commit message names [%s]' % i
    if not tests:
        return ('NO TEST', why) if why else ('FAIL', 'no test is tagged [%s] and no commit says `[%s] no test: why`' % (i, i))
    o, n = [old.get(key(t)) for t in tests], [new.get(key(t)) for t in tests]
    if any(x in ('no unity', None) for x in o + n):
        return 'UNCHECKED', 'its tests need Unity and none was given'
    if 'missing' in n:
        return 'UNCHECKED', 'a tagged test did not run on the fix (not found by its name)'
    if any(x != 'green' for x in n):
        return 'FAIL', 'a tagged test is red on the fix'
    if 'red' in o:
        if any(old.get(key(t)) == 'red' and '[%s]' % i in (old_said or {}).get(key(t), '') for t in tests):
            return 'PROVED', 'red on the old code with a failure that names [%s], green on the fix' % i
        return 'RED', ('red on the old code, green on the fix; its failure there does not name [%s], so it may be '
                       'red for another change of the same unit: read what it said' % i)
    if 'compile' in o or 'missing' in o:
        return 'WEAK', ('not shown red on the old code: the test did not run there (the old code does not compile '
                        'with it, or its runner stopped before it). Not proof of the bug')
    return 'FAIL', ('its test is green on the old code too: the test cannot fail. If the fix it tests is already in '
                    'the base, say `[%s] fix in base: <commit>` in a commit message' % i)


def main(argv=None):
    del NOTES[:]
    ap = argparse.ArgumentParser(description='did these commits fix what they say?')
    ap.add_argument('--tree', required=True)
    ap.add_argument('--base', required=True)
    ap.add_argument('--head', required=True)
    ap.add_argument('--lane', required=True)
    ap.add_argument('--ids', nargs='+', required=True)
    ap.add_argument('--unit', default='')
    ap.add_argument('--out')
    ap.add_argument('--unity', default=os.environ.get('TW_UNITY') or (str(UNITY) if UNITY.exists() else ''))
    ap.add_argument('--no-run', action='store_true', help='only what git can say: ids, BOMs, lane')
    ap.add_argument('--fix-in-base', action='append', default=[], metavar='ID=COMMIT',
                    help='the fix this id tests is a commit already in the base: run its tests against the code '
                         'before that commit (a commit message may say the same: `[ID] fix in base: <commit>`)')
    a = ap.parse_args(argv)
    tree = Path(a.tree).resolve()
    if 'fixcheck' not in tree.name.lower():
        raise SystemExit('fixcheck: %s is not a checkout of fixcheck\'s own (its folder name must hold "fixcheck"): '
                         'this script moves HEAD and resets the tree' % tree)
    if git(['status', '--porcelain'], tree).strip():
        raise SystemExit('fixcheck: %s has uncommitted files: it must be clean' % tree)
    base, head = (git(['rev-parse', '--verify', x + '^{commit}'], tree).strip() for x in (a.base, a.head))
    files = changed(tree, base, head)
    paths = [p for _, p in files]
    messages = git(['log', '--format=%B', '%s..%s' % (base, head)], tree)
    git(['checkout', '-q', '--detach', head], tree)
    tagged = {i: [] for i in a.ids}
    for status, path in files:
        if status != 'D' and is_test(path):
            text = (tree / path).read_text(encoding='utf-8-sig', errors='replace')
            for i, ts in find_tests(path, text, a.ids).items():
                tagged[i] += [t for t in ts if t not in tagged[i]]
    tests = [t for ts in tagged.values() for t in ts]
    tests = [t for n, t in enumerate(tests) if t not in tests[:n]]
    earlier = dict(x.split('=', 1) for x in a.fix_in_base)
    for i in a.ids:
        m = re.search(r'\[%s\]\s+fix in base:\s*([0-9a-f]{7,40})' % re.escape(i), messages)
        if m and i not in earlier:
            earlier[i] = m.group(1)
    earlier = {i: git(['rev-parse', '--verify', c + '^{commit}'], tree).strip() for i, c in earlier.items()}
    old, new, old_said = {}, {}, {}
    SAID.clear()
    if tests and not a.no_run:
        for fix in sorted(set(earlier.get(i) or '' for i in a.ids if tagged[i])):     # one old tree per earlier fix
            batch = [t for i in a.ids if (earlier.get(i) or '') == fix for t in tagged[i]]
            try:
                old_code(tree, base, head, files, fix or None)
                old.update(run_all(tree, [t for n, t in enumerate(batch) if t not in batch[:n]], a.unity))
            finally:
                git(['update-ref', '--no-deref', 'HEAD', head], tree)
                git(['reset', '-q', '--hard', head], tree)
        old_said = dict(SAID)
        new = run_all(tree, tests, a.unity)
    ids = {}
    for i in a.ids:
        why = re.search(r'\[%s\]\s+no test:\s*(.+)' % re.escape(i), messages)
        v, said = verdict_of(i, tagged[i], old, new, '[%s]' % i in messages, why.group(1).strip() if why else '',
                             old_said)
        if a.no_run and tagged[i] and v != 'FAIL':
            v, said = 'UNCHECKED', 'not run (--no-run)'
        if earlier.get(i) and v in ('PROVED', 'RED', 'WEAK'):
            said += ' (the old code: before %s, a fix already in the base)' % earlier[i][:8]
        ids[i] = {'verdict': v, 'why': said, 'fix_in_base': earlier.get(i, ''),
                  'tests': [dict(t, old=old.get(key(t)), new=new.get(key(t)), old_said=old_said.get(key(t), ''))
                            for t in tagged[i]]}
    boms, outside = gained_bom(tree, base, head, files), outside_lane(kind_of(a.lane), paths)
    got = [x['verdict'] for x in ids.values()]
    verdict = ('FAIL' if 'FAIL' in got or boms or outside else 'UNCHECKED' if 'UNCHECKED' in got else 'PASS')
    rec = {'unit': a.unit, 'lane': a.lane, 'base': base, 'head': head, 'verdict': verdict, 'ids': ids,
           'gained_bom': boms, 'outside_lane': outside, 'files': len(files), 'notes': NOTES[:8],
           'counts': {v: got.count(v) for v in ('PROVED', 'RED', 'WEAK', 'NO TEST', 'UNCHECKED', 'FAIL')},
           'checked_at': datetime.datetime.now().strftime('%Y-%m-%d %H:%M'), 'unity': bool(a.unity) and not a.no_run}
    out = Path(a.out) if a.out else (DRIVE / (a.unit + '.json') if a.unit and DRIVE.parent.is_dir() else None)
    if out:
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(json.dumps(rec, indent=2, sort_keys=True) + '\n', encoding='utf-8', newline='\n')
    print('%s  %s  %s..%s  %d files' % (verdict, a.unit or a.lane, base[:8], head[:8], len(files)))
    for i in a.ids:
        print('  %-9s [%s] %s' % (ids[i]['verdict'], i, ids[i]['why']))
        for t in ids[i]['tests']:
            print('            %s  old %s, fix %s' % (t['name'], t['old'], t['new']))
            if t['old_said']:
                print('                on the old code it said: %s' % t['old_said'][:160])
    for n in NOTES[:8]:
        print('  note      %s' % n)
    for p in boms:
        print('  FAIL      gained a UTF-8 BOM: %s' % p)
    for p in outside:
        print('  FAIL      outside a %s lane: %s' % (kind_of(a.lane), p))
    if out:
        print('written: %s' % out)
    return {'PASS': 0, 'FAIL': 1, 'UNCHECKED': 2}[verdict]


if __name__ == '__main__':
    sys.exit(main())
