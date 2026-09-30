"""A/B a code change on a deterministic report, and prove a new test fails on the code before it.

    python Tools/abtest.py bench --file <repo path> [--file ...] --variant NAME=SRC[,SRC...] [--variant ...]
                           [--filter TW.Tests.BehaviourBenchTests.Report_HowEveryUnitBehaves] [--seeds 1,2,3,4,5,6]
    python Tools/abtest.py fails-on-old --file <repo path> [--file ...] --filter <test name or class> [--old REF]
    python Tools/abtest.py gym --gym "<gym options>" [--file <repo path> --variant NAME=SRC ...] [--noise]

bench: runs the report test once per variant with the variant's source(s) copied over the --file(s), in order, and
once on the working tree ("now") unless a variant is named that; every run writes <run dir>/<name>.json (the test
reads the stem from TW_BENCH_OUT). Then it prints each metric per variant against "now", paired seed by seed, and
says whether every seed moved the same way. The report is deterministic (same code, same seeds, same numbers) but
chaotic: a change moves every later tick, so numbers it does not act on scatter across seeds like noise, and with 3
seeds an unrelated number agrees on all of them 1 time in 4. Believe what every seed agrees on, with --seeds giving
six or more for anything match-wide (--seeds sets TW_BENCH_SEEDS for the report).
fails-on-old: runs the named test(s) on the working tree and again with the --file(s) as git has them at --old
(HEAD by default: run it before committing the change; after, pass --old HEAD~1). A new
regression test must pass on the first and fail on the second, or it guards nothing (two of three hand-built tests
for one fix, 2026-09-29, measured the same on both).
gym: the visual A/B. Runs the gym (Editor/Gym.cs: each entry staged in the battle's own drawing and shot at its zoom
bands) with the same options on the working tree ("now"), again on it when --noise is given ("now2": the gym's own
run-to-run noise, the floor for everything else), and on each variant. Numbers first: per entry and band, the share of
pixels that changed against "now" (a channel off by more than 16) beside the noise floor, and every sidecar number
that moved (consequence, events, flags). Then blind pairs for a critic or the owner: pairs/<variant>/<entry>.jpg holds
"now" and the variant stacked in a seeded random order, and key.json (kept apart) says which is on top. Judge the pair
before opening the key; a critic judges each pair twice, the second time with the halves swapped.

The files are always put back (the working tree's own copies), even on Ctrl-C. Run from trench-warfare-3d/ with this
checkout's editor closed; it waits on Tools/editor_lock.py like the gate. Runs go to
%LOCALAPPDATA%\\TrenchWarfare\\abtest\\<yyyyMMdd-HHmm>\\ (outside every checkout; the newest 8 are kept).
"""
import argparse, datetime, json, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.path.dirname(HERE)
REPO = os.path.dirname(PROJECT)
DEFAULT_FILTER = 'TW.Tests.BehaviourBenchTests.Report_HowEveryUnitBehaves'


def editor():
    version = open(os.path.join(PROJECT, 'ProjectSettings', 'ProjectVersion.txt')).read()
    v = re.search(r'm_EditorVersion:\s*(\S+)', version).group(1)
    exe = os.path.join(os.environ.get('ProgramFiles', r'C:\Program Files'), 'Unity', 'Hub', 'Editor', v, 'Editor', 'Unity.exe')
    if not os.path.exists(exe):
        sys.exit(f'abtest: Unity {v} is not at {exe}')
    return exe


def run_dir():
    root = os.path.join(os.environ.get('LOCALAPPDATA', os.path.expanduser('~')), 'TrenchWarfare', 'abtest')
    os.makedirs(root, exist_ok=True)
    old = sorted(d for d in os.listdir(root) if os.path.isdir(os.path.join(root, d)))
    for d in old[:-7]:
        shutil.rmtree(os.path.join(root, d), ignore_errors=True)
    path = os.path.join(root, datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
    os.makedirs(path)
    return path


def run_tests(test_filter, results, log, env_extra=None):
    """One batch EditMode run; returns (passed, failed) from the results XML (the XML is the verdict, as in gate.ps1)."""
    subprocess.run([sys.executable, os.path.join(HERE, 'editor_lock.py'), 'wait', '--timeout', '3600'], check=False)
    env = dict(os.environ, **(env_extra or {}))
    if os.path.exists(results):
        os.remove(results)
    subprocess.run([editor(), '-batchmode', '-runTests', '-projectPath', PROJECT, '-testPlatform', 'EditMode',
                    '-testFilter', test_filter, '-testResults', results, '-logFile', log], env=env, timeout=5400)
    if not os.path.exists(results):
        return None
    xml = open(results, encoding='utf-8', errors='replace').read()
    m = re.search(r'<test-run [^>]*passed="(\d+)"[^>]*failed="(\d+)"', xml)
    return (int(m.group(1)), int(m.group(2))) if m else None


class Swap:
    """Copies sources over repo files and always puts the working tree's copies back."""

    def __init__(self, files):
        self.files = [os.path.join(REPO, f) for f in files]
        self.saved = [open(f, 'rb').read() if os.path.exists(f) else None for f in self.files]

    def put(self, contents):
        for f, c in zip(self.files, contents):
            open(f, 'wb').write(c)

    def restore(self):
        for f, c in zip(self.files, self.saved):
            if c is None:
                if os.path.exists(f):
                    os.remove(f)
            else:
                open(f, 'wb').write(c)


def head_version(rel, ref='HEAD'):
    r = subprocess.run(['git', '-C', REPO, 'show', ref + ':' + rel.replace('\\', '/')], capture_output=True)
    if r.returncode != 0:
        sys.exit(f'abtest: {rel} is not in {ref}')
    return r.stdout


def bench(args):
    variants = []
    for v in args.variant:
        name, _, srcs = v.partition('=')
        paths = srcs.split(',')
        if len(paths) != len(args.file):
            sys.exit(f'abtest: variant {name} gives {len(paths)} source(s) for {len(args.file)} --file(s)')
        variants.append((name, [open(p, 'rb').read() for p in paths]))
    swap = Swap(args.file)
    if 'now' not in [n for n, _ in variants]:
        variants.insert(0, ('now', swap.saved))
    out = run_dir()
    print(f'abtest: {len(variants)} runs of {args.filter} into {out}')
    try:
        for name, contents in variants:
            swap.put(contents)
            env = {'TW_BENCH_OUT': os.path.join(out, name)}
            if args.seeds:
                env['TW_BENCH_SEEDS'] = args.seeds
            got = run_tests(args.filter, os.path.join(out, name + '.xml'), os.path.join(out, name + '.log'), env)
            print(f'  {name}: {"no verdict (compile error? see " + name + ".log)" if got is None else f"{got[0]} passed, {got[1]} failed"}')
    finally:
        swap.restore()
    report(out, [n for n, _ in variants])


def report(out, names):
    data = {}
    for n in names:
        p = os.path.join(out, n + '.json')
        if os.path.exists(p):
            data[n] = json.load(open(p))
    if 'now' not in data:
        sys.exit('abtest: the working tree\'s run wrote no report')
    seeds = [k for k in data['now'] if k != 'mean']
    metrics = list(data['now']['mean'])
    cols = [n for n in names if n in data]
    print('\n' + f'{"metric":30}' + ''.join(f'{n:>14}' for n in cols) + '   seeds agree')
    for m in metrics:
        row = f'{m:30}' + ''.join(f'{data[n]["mean"].get(m, float("nan")):14.3f}' for n in cols)
        agree = []
        for n in cols[1:]:
            d = [data[n][s].get(m, 0) - data['now'][s].get(m, 0) for s in seeds]
            if all(x == 0 for x in d):
                agree.append(f'{n}: same')
            elif all(x > 0 for x in d) or all(x < 0 for x in d):
                agree.append(f'{n}: all {"up" if d[0] > 0 else "down"}')
            else:
                agree.append(f'{n}: mixed')
        print(row + '   ' + '; '.join(agree))
    json.dump(data, open(os.path.join(out, 'abtest.json'), 'w'), indent=1)


def fails_on_old(args):
    out = run_dir()
    swap = Swap(args.file)
    try:
        now = run_tests(args.filter, os.path.join(out, 'now.xml'), os.path.join(out, 'now.log'))
        swap.put([head_version(f, args.old) for f in args.file])
        old = run_tests(args.filter, os.path.join(out, 'old.xml'), os.path.join(out, 'old.log'))
    finally:
        swap.restore()
    print(f'now: {now}  old ({args.old}): {old}   (passed, failed)')
    if now is None or old is None:
        sys.exit('abtest: a run gave no verdict (compile error? see the logs in ' + out + ')')
    if now[1] == 0 and old[1] > 0:
        print('GOOD: it passes on the change and fails without it')
        return
    sys.exit('BAD: ' + ('it fails on the change' if now[1] else 'it passes without the change too: it guards nothing'))


def gym_run(options, out, name):
    """One unattended gym run into <out>/<name>/run (its own parent, so the gym's pruning of sibling runs cannot touch
    another variant's). Returns the run folder, or None."""
    run = os.path.join(out, name, 'run')
    os.makedirs(os.path.dirname(run), exist_ok=True)
    subprocess.run([sys.executable, os.path.join(HERE, 'editor_lock.py'), 'wait', '--timeout', '3600'], check=False)
    r = subprocess.run([editor(), '-batchmode', '-projectPath', PROJECT, '-executeMethod', 'TW.Editor.Gym.CommandLine',
                        '-twgym', f'{options} out={run}', '-logFile', os.path.join(out, name + '.log')], timeout=5400)
    ok = os.path.exists(os.path.join(run, 'summary.json'))
    print(f'  {name}: gym exit {r.returncode}{"" if ok else " (no summary.json: see " + name + ".log)"}')
    return run if ok else None


def sheets(run):
    found = {}
    for tab in sorted(os.listdir(run)):
        d = os.path.join(run, tab)
        if os.path.isdir(d) and tab != 'raw':
            for f in sorted(os.listdir(d)):
                if f.endswith('.jpg'):
                    found[tab + '/' + f[:-4]] = os.path.join(d, f)
    return found


def band_count(path):
    """The gym tiles 480 px wide bands: six for bands=all, three for bands=close."""
    from PIL import Image
    return max(1, round(Image.open(path).width / 480))


def band_change(a, b, bands):
    """Per band: the share of pixels with a channel more than 16 apart (1.0 when the sheets differ in size)."""
    from PIL import Image, ImageChops
    ia, ib = Image.open(a).convert('RGB'), Image.open(b).convert('RGB')
    if ia.size != ib.size:
        return [1.0] * bands
    w = ia.width // bands
    out = []
    for k in range(bands):
        box = (k * w, 0, (k + 1) * w, ia.height)
        diff = ImageChops.difference(ia.crop(box), ib.crop(box)).convert('L').point(lambda v: 255 if v > 16 else 0)
        out.append(diff.histogram()[255] / float(w * ia.height))
    return out


def flat(d, prefix=''):
    got = {}
    for k, v in d.items():
        if isinstance(v, dict):
            got.update(flat(v, prefix + k + '.'))
        elif isinstance(v, (int, float)) and not isinstance(v, bool):
            got[prefix + k] = v
    return got


def gym(args):
    import random
    from PIL import Image
    variants = [('now', None)]
    if args.noise:
        variants.append(('now2', None))
    for v in args.variant or []:
        name, _, srcs = v.partition('=')
        paths = srcs.split(',')
        if len(paths) != len(args.file or []):
            sys.exit(f'abtest: variant {name} gives {len(paths)} source(s) for {len(args.file or [])} --file(s)')
        variants.append((name, [open(p, 'rb').read() for p in paths]))
    if len(variants) < 2:
        sys.exit('abtest gym: give --noise, or --file with --variant, or both')
    swap = Swap(args.file or [])
    out = run_dir()
    print(f'abtest: {len(variants)} gym runs of "{args.gym}" into {out}')
    runs = {}
    try:
        for name, contents in variants:
            swap.put(contents if contents is not None else swap.saved)
            got = gym_run(args.gym, out, name)
            if got:
                runs[name] = got
    finally:
        swap.restore()
    if 'now' not in runs:
        sys.exit("abtest gym: the working tree's run failed")
    base = sheets(runs['now'])
    names = [n for n, _ in variants if n in runs and n != 'now']
    others = {n: sheets(runs[n]) for n in names}
    summary = {'options': args.gym, 'runs': runs, 'entries': {}}
    rng = random.Random(1917)
    key = {}
    print(f'\n{"entry":34} {"band":>4} ' + ''.join(f'{n:>10}' for n in names) + '   (share of pixels changed against now)')
    for entry, path in base.items():
        side = json.load(open(path[:-4] + '.json'))
        bands = band_count(path)
        row = {n: (band_change(path, others[n][entry], bands) if entry in others[n] else None) for n in names}
        summary['entries'][entry] = row
        for k in range(bands):
            cells = ''.join(f'{(row[n][k] if row[n] else float("nan")):10.4f}' for n in names)
            print(f'{entry:34} {k:>4} {cells}')
        now_nums = flat({'consequence': side.get('consequence', {}), 'events': side.get('events', {})})
        for n in names:
            o = others[n].get(entry)
            if not o:
                print(f'    {n}: no sheet for this entry')
                continue
            oside = json.load(open(o[:-4] + '.json'))
            other_nums = flat({'consequence': oside.get('consequence', {}), 'events': oside.get('events', {})})
            moved = {k2: (now_nums.get(k2, 0), other_nums.get(k2, 0)) for k2 in set(now_nums) | set(other_nums)
                     if now_nums.get(k2, 0) != other_nums.get(k2, 0)}
            flags_moved = side.get('flags') != oside.get('flags')
            if moved or flags_moved:
                print(f'    {n}: ' + ', '.join(f'{k2} {x}->{y}' for k2, (x, y) in sorted(moved.items()))
                      + (f'  flags {side.get("flags")} -> {oside.get("flags")}' if flags_moved else ''))
            if n == 'now2':
                continue
            top_is_now = rng.random() < 0.5
            a, b = Image.open(path).convert('RGB'), Image.open(o).convert('RGB')
            first, second = (a, b) if top_is_now else (b, a)
            pair = Image.new('RGB', (max(a.width, b.width), a.height + b.height + 8), (255, 255, 255))
            pair.paste(first, (0, 0)); pair.paste(second, (0, first.height + 8))
            pdir = os.path.join(out, 'pairs', n); os.makedirs(pdir, exist_ok=True)
            pair.save(os.path.join(pdir, entry.replace('/', '_') + '.jpg'), quality=90)
            key.setdefault(n, {})[entry] = 'top=now' if top_is_now else 'top=' + n
    json.dump(summary, open(os.path.join(out, 'abtest-gym.json'), 'w'), indent=1)
    if key:
        json.dump(key, open(os.path.join(out, 'key.json'), 'w'), indent=1)
        print(f'\nblind pairs in {os.path.join(out, "pairs")}; the key is {os.path.join(out, "key.json")} (open it after judging)')


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    sub = ap.add_subparsers(dest='cmd', required=True)
    b = sub.add_parser('bench')
    b.add_argument('--file', action='append', required=True, help='repo-relative path swapped per variant')
    b.add_argument('--variant', action='append', required=True, help='NAME=SRC[,SRC...] (one source per --file)')
    b.add_argument('--filter', default=DEFAULT_FILTER)
    b.add_argument('--seeds', help='comma-separated seeds for the report (TW_BENCH_SEEDS); its own default otherwise')
    f = sub.add_parser('fails-on-old')
    f.add_argument('--file', action='append', required=True, help='repo-relative path the change touched')
    f.add_argument('--filter', required=True)
    f.add_argument('--old', default='HEAD', help='the commit whose copy of the file(s) the test must fail on')
    g = sub.add_parser('gym')
    g.add_argument('--gym', required=True, help='the gym options, e.g. "tabs=scenes,units filter=Maw|Kettle"')
    g.add_argument('--file', action='append', help='repo-relative path swapped per variant')
    g.add_argument('--variant', action='append', help='NAME=SRC[,SRC...] (one source per --file)')
    g.add_argument('--noise', action='store_true', help='run the working tree twice: the gym run-to-run noise floor')
    args = ap.parse_args()
    os.chdir(PROJECT)
    {'bench': bench, 'fails-on-old': fails_on_old, 'gym': gym}[args.cmd](args)


if __name__ == '__main__':
    main()
