#!/usr/bin/env python3
"""The balance sweep: play variants of a number over the same seeds, and say what each did to the fight.

    python Tools/sweep.py run <spec.json> [--seeds 1,2,3,4,5,6,7,8] [--minutes N] [--chunk 4] [--resume <run folder>]
    python Tools/sweep.py report <run folder>
    python Tools/sweep.py compile <spec.json> <out.json>

The behaviour bench says how units behave; this says what a number does to the match. A spec names a scenario and
variants; every variant is DATA (a unit's number, the match's config, the script's knobs), so nothing is recompiled and
nothing is committed: BalanceSweepTests.Report_TheSweep writes each variant into the match before its first tick.

    {"scenario": "match",                  "match": the scene's match, the script on both seats; "ladder": the assault ladder
     "policy": "Script", "minutes": 8,     match only; a policy other than Script plays seat 0 and never swaps seats
     "swap_seats": true, "heroes": false,  every seed twice, the sides changing seats; heroes are a seat's edge, off
     "seeds": [1, 2, 3, 4, 5, 6, 7, 8],
     "base": [<patch>, ...],               on every variant, the baseline "now" too
     "ladder": [{"attackers": 30, "defenders": 10, "support": "None"}],
     "variants": [{"name": "mg", "patches": [{"unit": "Machinegunner", "field": "Weapon.Damage", "mul": [0.8, 0.9]}]}],
     "targets": {"win_a": [0.4, 0.6]}}     the band a metric's mean should sit in; none: it only reports

A patch is one of {"unit": NAME, "field": PATH, "set"|"mul": V}, {"config": FIELD, "set"|"mul": V},
{"script_a"|"script_b": FIELD, "set"|"mul": V}. A LIST for V is a grid: the variant becomes one per value, named
name@value (two grids in one variant: every pair). Side "a" is whoever the config's FactionA / LoadoutA and script_a
belong to, on either seat. A wrong unit, field or value fails the run with its name.

run: the baseline ("now": the base patches only) and every variant, --chunk variants to an editor launch, into
%LOCALAPPDATA%\\TrenchWarfare\\sweeps\\<yyyyMMdd-HHmm>-<sha>\\ (outside every checkout; the newest 8 kept, a folder
holding KEEP.txt always). A variant whose report is there is not played again, so --resume on a run that died carries
on where it stopped (it refuses a spec that changed). Run from trench-warfare-3d/ with this checkout's editor closed:
it waits on Tools/editor_lock.py like the gate. A long sweep goes under Tools/pipeline/run_detached.py.

report: per metric, the baseline's mean and its p10-p90 over the seeds, each variant's mean, and whether every seed
moved the same way (the bench's rule: deterministic but chaotic, so believe what every seed agrees on). Against the
targets, each variant is in or out of each band. report.md and report.json go beside the runs.
Exit 0 the baseline is inside every band (or there are none), 2 it is outside one, 1 could not run.

compile: the flat spec the test reads, for a SOURCE variant (a constant, not a table entry) through abtest.py:
    TW_SWEEP=<out.json> python Tools/abtest.py bench --filter TW.Tests.BalanceSweepTests.Report_TheSweep --file ... --variant ...
(with no TW_SWEEP_OUT the test plays the compiled spec's first variant and writes abtest's own report file).
Stdlib only.
"""
import argparse, datetime, hashlib, itertools, json, os, re, shutil, subprocess, sys, time

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import abtest  # noqa: E402  (the batch test runner, the editor lock and the results XML as the verdict)

PROJECT, REPO = abtest.PROJECT, abtest.REPO
FILTER = 'TW.Tests.BalanceSweepTests.Report_TheSweep'
KEEP = 8
SIDES = ('unit', 'config', 'script_a', 'script_b')
NAME = re.compile(r'^[A-Za-z0-9][A-Za-z0-9._=@+-]*$')
SPEC_KEYS = {'about', 'scenario', 'policy', 'minutes', 'swap_seats', 'heroes', 'seeds', 'base', 'ladder', 'variants', 'targets'}
RUNG_KEYS = {'attackers', 'defenders', 'support', 'gunners', 'attack_guns', 'guns_cover'}


class SpecError(Exception):
    pass


# ---- the spec ---------------------------------------------------------------------------------------------------------

def text(v):
    """A value as the test parses it: invariant numbers, true/false, a list as a comma-separated string."""
    if isinstance(v, bool):
        return 'true' if v else 'false'
    if isinstance(v, (int, float)):
        return repr(v)
    if isinstance(v, str):
        return v
    raise SpecError(f'a value is {type(v).__name__}, not a number, a switch or a name')


def patch_forms(p, where):
    """One written patch as (flat patch without its value, op, the values it takes: one, or a grid's)."""
    if not isinstance(p, dict):
        raise SpecError(f'{where}: a patch is an object')
    on = [s for s in SIDES if s in p]
    if len(on) != 1:
        raise SpecError(f'{where}: a patch names exactly one of {", ".join(SIDES)}: {p}')
    ops = [o for o in ('set', 'mul') if o in p]
    if len(ops) != 1:
        raise SpecError(f'{where}: a patch has exactly one of set, mul: {p}')
    extra = set(p) - {on[0], ops[0], 'field'}
    if extra:
        raise SpecError(f'{where}: a patch has no {", ".join(sorted(extra))}: {p}')
    if on[0] == 'unit':
        if not isinstance(p.get('field'), str):
            raise SpecError(f'{where}: a unit patch names its field: {p}')
        flat = {'on': 'unit', 'unit': str(p['unit']), 'field': p['field']}
    else:
        if 'field' in p:
            raise SpecError(f'{where}: "{on[0]}" is the field itself, there is no "field": {p}')
        flat = {'on': on[0], 'unit': '', 'field': str(p[on[0]])}
    v = p[ops[0]]
    grid = isinstance(v, list)
    if grid and not v:
        raise SpecError(f'{where}: an empty grid: {p}')
    return flat, ops[0], [text(x) for x in v] if grid else [text(v)], grid


def expand(variant, base):
    """A written variant as its flat variants: one, or one per point of its grids."""
    name = variant.get('name')
    if not isinstance(name, str) or not NAME.match(name) or name == 'now':
        raise SpecError(f'a variant\'s name is letters, digits and ._=@+- and not "now": {name!r}')
    forms = [patch_forms(p, name) for p in variant.get('patches', [])]
    if not forms:
        raise SpecError(f'{name}: a variant with no patch is the baseline')
    out = []
    for point in itertools.product(*[values for _, _, values, _ in forms]):
        tag = ''.join('@' + v for (_, _, _, grid), v in zip(forms, point) if grid)
        patches = list(base) + [dict(flat, op=op, value=v) for (flat, op, _, _), v in zip(forms, point)]
        out.append({'name': name + tag, 'patches': patches})
    return out


def compile_spec(spec):
    """The written spec as (header, variants): flat, every value a string, "now" first. Raises SpecError."""
    unknown = set(spec) - SPEC_KEYS
    if unknown:
        raise SpecError(f'the spec has no {", ".join(sorted(unknown))}')
    scenario = spec.get('scenario', 'match')
    if scenario not in ('match', 'ladder'):
        raise SpecError(f'scenario is match or ladder, not {scenario!r}')
    policy = spec.get('policy', 'Script')
    swap = bool(spec.get('swap_seats', True)) and scenario == 'match'
    if swap and policy != 'Script':
        if 'swap_seats' in spec:
            raise SpecError(f'seats swap only with the script on both (policy {policy})')
        swap = False
    ladder = []
    for r in spec.get('ladder', []):
        extra = set(r) - RUNG_KEYS
        if extra:
            raise SpecError(f'a rung has no {", ".join(sorted(extra))}')
        ladder.append({'attackers': int(r['attackers']), 'defenders': int(r['defenders']), 'support': str(r.get('support', 'None')),
                       'gunners': int(r.get('gunners', 0)), 'attackGuns': int(r.get('attack_guns', 0)), 'gunsCover': bool(r.get('guns_cover', False))})
    if scenario == 'ladder' and not ladder:
        raise SpecError('the ladder scenario names no rung')
    base = []
    for p in spec.get('base', []):
        flat, op, values, grid = patch_forms(p, 'base')
        if grid:
            raise SpecError(f'base: a grid belongs in a variant: {p}')
        base.append(dict(flat, op=op, value=values[0]))
    variants = [{'name': 'now', 'patches': list(base)}]
    for v in spec.get('variants', []):
        variants += expand(v, base)
    names = [v['name'] for v in variants]
    twice = sorted({n for n in names if names.count(n) > 1})
    if twice:
        raise SpecError(f'two variants are called {", ".join(twice)}')
    for m, band in spec.get('targets', {}).items():
        if not (isinstance(band, list) and len(band) == 2 and band[0] <= band[1]):
            raise SpecError(f'target {m} is [low, high]: {band}')
    header = {'scenario': scenario, 'policy': policy, 'minutes': int(spec.get('minutes', 8)), 'heroes': bool(spec.get('heroes', False)),
              'swapSeats': swap, 'ladder': ladder}
    return header, variants


def fingerprint(header, variants, seeds):
    return hashlib.sha256(json.dumps([header, variants, seeds], sort_keys=True).encode()).hexdigest()[:16]


# ---- the numbers ------------------------------------------------------------------------------------------------------

def percentile(values, q):
    """Linear between the ranks, as numpy's default: percentile([1, 2, 3, 4], 0.5) is 2.5."""
    s = sorted(values)
    if not s:
        return float('nan')
    at = (len(s) - 1) * q
    lo = int(at)
    hi = min(lo + 1, len(s) - 1)
    return s[lo] + (s[hi] - s[lo]) * (at - lo)


def agreement(deltas):
    """How the seeds moved, a variant against the baseline: the bench's rule."""
    if not deltas:
        return 'no seed has both'
    if all(d == 0 for d in deltas):
        return 'same'
    if all(d > 0 for d in deltas):
        return 'all up'
    if all(d < 0 for d in deltas):
        return 'all down'
    return 'mixed'


def in_band(value, band):
    return value is not None and band[0] <= value <= band[1]


def compare(data, names, targets):
    """Rows for the report: per metric the baseline's spread, and each variant against it."""
    now = data['now']
    seeds = [k for k in now if k != 'mean']
    metrics = list(now['mean'])
    for n in names:
        metrics += [m for m in data.get(n, {}).get('mean', {}) if m not in metrics]
    rows = []
    for m in metrics:
        base = [now[s][m] for s in seeds if m in now[s]]
        row = {'metric': m, 'now': now['mean'].get(m), 'p10': percentile(base, 0.1), 'p90': percentile(base, 0.9), 'n': len(base),
               'band': targets.get(m), 'variants': {}}
        if m in targets:
            row['now_in'] = in_band(row['now'], targets[m])
        for n in names:
            if n == 'now' or n not in data:
                continue
            v = data[n]
            deltas = [v[s][m] - now[s][m] for s in seeds if s in v and m in v[s] and m in now[s]]
            cell = {'mean': v['mean'].get(m), 'seeds': agreement(deltas)}
            if m in targets:
                cell['in'] = in_band(cell['mean'], targets[m])
            row['variants'][n] = cell
        rows.append(row)
    return rows


def fmt(v):
    return '-' if v is None or v != v else f'{v:.3f}'


def render(rows, names, targets, meta):
    others = [n for n in names if n != 'now']
    lines = [f'# Sweep {meta.get("spec", "")}', '',
             f'{meta.get("commit", "?")}{" (dirty)" if meta.get("dirty") else ""}, seeds {meta.get("seeds", "?")}, {meta.get("scenario", "")}', '',
             '| metric | now | p10-p90 | ' + ' | '.join(others) + ' | band |', '|---|---|---|' + '---|' * (len(others) + 1)]
    for r in rows:
        cells = []
        for n in others:
            c = r['variants'].get(n)
            cells.append('-' if c is None else f'{fmt(c["mean"])} ({c["seeds"]}){"" if "in" not in c else " IN" if c["in"] else " OUT"}')
        band = '' if r['band'] is None else f'{r["band"][0]}-{r["band"][1]} now {"IN" if r["now_in"] else "OUT"}'
        lines.append(f'| {r["metric"]} | {fmt(r["now"])} | {fmt(r["p10"])}-{fmt(r["p90"])} | ' + ' | '.join(cells) + f' | {band} |')
    missing = [m for m in targets if m not in {r['metric'] for r in rows}]
    if missing:
        lines += ['', 'Targets naming a metric no run reported: ' + ', '.join(missing)]
    if targets:
        lines += ['', '| variant | targets in band |', '|---|---|']
        for n in names:
            got = [(r['now_in'] if n == 'now' else r['variants'].get(n, {}).get('in')) for r in rows if r['band'] is not None]
            lines.append(f'| {n} | {sum(1 for g in got if g)} of {len(targets)} |')
    return '\n'.join(lines) + '\n'


def report(out):
    """Print and write the report of a run folder. Returns the exit code."""
    meta_path = os.path.join(out, 'run.json')
    if not os.path.exists(meta_path):
        print(f'sweep: {out} is not a sweep run (no run.json)')
        return 1
    meta = json.load(open(meta_path))
    names = meta['variants']
    data = {n: json.load(open(os.path.join(out, n + '.json'))) for n in names if os.path.exists(os.path.join(out, n + '.json'))}
    if 'now' not in data:
        print('sweep: the baseline wrote no report (a compile error? see chunk-0.log)')
        return 1
    targets = meta.get('targets', {})
    rows = compare(data, names, targets)
    md = render(rows, names, targets, meta)
    not_run = [n for n in names if n not in data]
    if not_run:
        md += '\nNot played: ' + ', '.join(not_run) + ' (sweep.py run <spec> --resume <this folder>)\n'
    open(os.path.join(out, 'report.md'), 'w', encoding='utf-8', newline='\n').write(md)
    json.dump({'meta': meta, 'rows': rows, 'not_run': not_run}, open(os.path.join(out, 'report.json'), 'w', newline='\n'), indent=1)
    print(md)
    print(f'sweep: {os.path.join(out, "report.md")}')
    if not_run:
        return 1
    outside = [r['metric'] for r in rows if r['band'] is not None and not r['now_in']]
    outside += [m for m in targets if m not in {r['metric'] for r in rows}]
    return 2 if outside else 0


# ---- running ----------------------------------------------------------------------------------------------------------

def sweeps_root():
    root = os.path.join(os.environ.get('TW_SWEEPS') or os.path.join(os.environ.get('LOCALAPPDATA', os.path.expanduser('~')), 'TrenchWarfare', 'sweeps'))
    os.makedirs(root, exist_ok=True)
    return root


def prune(root, keep=KEEP):
    """Only folders that are sweep runs (run.json), never one holding KEEP.txt, and never the newest `keep`."""
    runs = sorted(d for d in os.listdir(root) if os.path.exists(os.path.join(root, d, 'run.json')))
    for d in runs[:-keep] if keep > 0 else runs:
        if not os.path.exists(os.path.join(root, d, 'KEEP.txt')):
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)


def git(*args):
    return subprocess.run(['git', '-C', REPO] + list(args), capture_output=True, text=True).stdout.strip()


def unity_runner(compiled_path, out, seeds, index):
    stem = os.path.join(out, f'chunk-{index}')
    return abtest.run_tests(FILTER, stem + '.xml', stem + '.log', {'TW_SWEEP': compiled_path, 'TW_SWEEP_OUT': out, 'TW_BENCH_SEEDS': seeds})


def refusals(out, index):
    """What the test said when it refused a variant: its own lines from the results XML, each once."""
    path = os.path.join(out, f'chunk-{index}.xml')
    if not os.path.exists(path):
        return []
    xml = open(path, encoding='utf-8', errors='replace').read()
    return sorted(set(m.strip() for m in re.findall(r'(sweep: [^\r\n\]<]+)', xml)))


def play(out, header, variants, seeds, chunk, runner=unity_runner):
    """Every variant with no report yet, `chunk` to a launch. Returns the names still without one."""
    pending = [v for v in variants if not os.path.exists(os.path.join(out, v['name'] + '.json'))]
    done = len(variants) - len(pending)
    first = len([f for f in os.listdir(out) if re.match(r'chunk-\d+\.json$', f)])
    for k in range(0, len(pending), max(1, chunk)):
        part = pending[k:k + max(1, chunk)]
        index = first + k // max(1, chunk)
        compiled = os.path.join(out, f'chunk-{index}.json')
        json.dump(dict(header, variants=part), open(compiled, 'w', newline='\n'), indent=1)
        began = time.time()
        got = runner(compiled, out, seeds, index)
        made = [v['name'] for v in part if os.path.exists(os.path.join(out, v['name'] + '.json'))]
        done += len(made)
        verdict = 'no verdict (compile error, or the editor never ran: see the log)' if got is None else f'{got[0]} passed, {got[1]} failed'
        print(f'sweep: chunk {index} ({", ".join(v["name"] for v in part)}): {verdict}; {len(made)} of {len(part)} reported, '
              f'{(time.time() - began) / 60:.1f} min; {done} of {len(variants)} done', flush=True)
        if len(made) < len(part):
            for line in refusals(out, index):
                print('  ' + line, flush=True)
            break   # a patch the test refused, or a dead editor: the rest would fail the same way
    return [v['name'] for v in variants if not os.path.exists(os.path.join(out, v['name'] + '.json'))]


def run(args, runner=unity_runner):
    try:
        spec = json.load(open(args.spec, encoding='utf-8'))
        if args.minutes:
            spec['minutes'] = args.minutes
        header, variants = compile_spec(spec)
    except (SpecError, ValueError, KeyError, OSError) as e:
        print(f'sweep: {args.spec}: {e}')
        return 1
    seeds = args.seeds or ','.join(str(s) for s in spec.get('seeds', [1, 2, 3, 4, 5, 6, 7, 8]))
    mark = fingerprint(header, variants, seeds)
    if args.resume:
        out = os.path.abspath(args.resume)
        if not os.path.exists(os.path.join(out, 'run.json')):
            print(f'sweep: {out} is not a sweep run')
            return 1
        if json.load(open(os.path.join(out, 'run.json'))).get('fingerprint') != mark:
            print('sweep: the spec, the seeds or the minutes changed since that run: start a new one')
            return 1
    else:
        root = sweeps_root()
        prune(root, KEEP - 1)
        sha = git('rev-parse', '--short', 'HEAD') or 'nogit'
        out = os.path.join(root, datetime.datetime.now().strftime('%Y%m%d-%H%M') + '-' + sha)
        os.makedirs(out, exist_ok=True)
        meta = {'spec': os.path.basename(args.spec), 'commit': sha, 'dirty': bool(git('status', '--porcelain')), 'seeds': seeds,
                'scenario': header['scenario'], 'minutes': header['minutes'], 'fingerprint': mark,
                'variants': [v['name'] for v in variants], 'targets': spec.get('targets', {}),
                'started': datetime.datetime.now().isoformat(timespec='seconds')}
        json.dump(meta, open(os.path.join(out, 'run.json'), 'w', newline='\n'), indent=1)
        shutil.copyfile(args.spec, os.path.join(out, 'spec.json'))
    print(f'sweep: {len(variants)} variants x seeds {seeds} into {out}', flush=True)
    play(out, header, variants, seeds, args.chunk, runner)
    return report(out)


def main(argv=None):
    ap = argparse.ArgumentParser(description='Play variants of a number over the same seeds and say what each did.')
    sub = ap.add_subparsers(dest='cmd', required=True)
    r = sub.add_parser('run')
    r.add_argument('spec')
    r.add_argument('--seeds', help='comma-separated; the spec\'s own, or 1..8')
    r.add_argument('--minutes', type=int, help='a match\'s length; the spec\'s own, or 8')
    r.add_argument('--chunk', type=int, default=4, help='variants to one editor launch')
    r.add_argument('--resume', help='a run folder to carry on in')
    p = sub.add_parser('report')
    p.add_argument('run')
    c = sub.add_parser('compile')
    c.add_argument('spec')
    c.add_argument('out')
    args = ap.parse_args(argv)
    if args.cmd == 'report':
        return report(os.path.abspath(args.run))
    if args.cmd == 'compile':
        try:
            header, variants = compile_spec(json.load(open(args.spec, encoding='utf-8')))
        except (SpecError, ValueError, KeyError, OSError) as e:
            print(f'sweep: {args.spec}: {e}')
            return 1
        json.dump(dict(header, variants=variants), open(args.out, 'w', newline='\n'), indent=1)
        print(f'sweep: {len(variants)} variants in {args.out} (the test plays the first when TW_SWEEP_OUT is not set)')
        return 0
    args.spec = os.path.abspath(args.spec)
    os.chdir(PROJECT)
    return run(args)


if __name__ == '__main__':
    sys.exit(main())
