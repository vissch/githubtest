#!/usr/bin/env python3
"""The night's run: the behaviour bench and the gym on this checkout, compared with the night before.

    python Tools/nightly.py [--seeds 1,2,3,4,5,6] [--gym "minutes=60"] [--keep 7] [--skip-bench] [--skip-gym]

The owner's loop (Brief 2: a bug catcher that loops round all the sims, and loops of learning): what the bench and the
gym say tonight, next to what they said last time, so a change nobody looked at is seen the next morning. It runs
whatever this checkout holds and records the commit and whether the tree was dirty; it does not fetch, switch or
build anything. Run from trench-warfare-3d/ with this checkout's editor closed (it waits on Tools/editor_lock.py, as
the gate does). Each night goes to %LOCALAPPDATA%\\TrenchWarfare\\nightly\\<yyyyMMdd-HHmm>-<sha>\\ (outside every
checkout; the newest --keep are kept): bench.json and bench.txt (BehaviourBenchTests), gym\\run\\ (Editor/Gym.cs, every
tab), report.md and report.json.

The report, against the newest earlier night that has the same part:
- bench: every metric's mean; with the same seeds, whether it moved the same way on every seed. A metric where more is
  worse (jams, spins, turn reversals, men stuck, idle, piled, deaths in clumps) that rose on every seed is a regression.
  The bench is deterministic but chaotic (BehaviourBenchTests): a number that moved on some seeds and not others is
  the match's own scatter, not a finding;
- gym: every flag raised tonight that was not raised last time, and per entry and band the share of pixels that changed
  (abtest.py's measure). The gym draws the same pictures twice (paced in game time, 0.00-0.06 % between runs of one
  commit), so a change on the same commit is a fault in the gym, and on a new commit is the new commit's to explain.

Exit 0 nothing new, 2 something to look at (a new flag, a bench regression, a picture that changed on the same
commit), 1 could not run. Scheduling it is the owner's call, not this tool's; a daily task would be, for example:
    schtasks /Create /SC DAILY /ST 03:00 /TN TW3D-nightly /TR "python <checkout>\\trench-warfare-3d\\Tools\\nightly.py"
"""
import argparse, datetime, json, os, shutil, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import abtest  # noqa: E402  (the bench and gym runners and the picture measure live there)

PROJECT, REPO = abtest.PROJECT, abtest.REPO
# metrics where a higher number is a worse match (BehaviourBenchTests' names); the rest are reported, never judged
WORSE_UP = ('machine_jam_s', 'machine_spin_s', 'machine_flips_per100', 'machine_worst_flips_per100', 'machine_worst_jam_s',
            'machine_worst_spin_s', 'men_stuck_s_per_man_min', 'men_worst_stuck_s', 'men_idle_open_share', 'men_piled_share',
            'deaths_in_clumps_share', 'biggest_clump')
SAME_COMMIT_NOISE = 0.002   # a band changed by more than this on an unchanged commit is the gym's fault (measured 0.0006)


def git(*args):
    r = subprocess.run(['git', '-C', REPO] + list(args), capture_output=True, text=True)
    return r.stdout.strip()


def night_root():
    root = os.path.join(os.environ.get('LOCALAPPDATA', os.path.expanduser('~')), 'TrenchWarfare', 'nightly')
    os.makedirs(root, exist_ok=True)
    return root


def previous(root, tonight, part):
    """The newest earlier night that holds `part` (bench.json, or gym\\run\\summary.json)."""
    for d in sorted((d for d in os.listdir(root) if d < tonight), reverse=True):
        if os.path.exists(os.path.join(root, d, part)):
            return os.path.join(root, d)
    return None


def bench_part(night, seeds):
    stem = os.path.join(night, 'bench')
    got = abtest.run_tests(abtest.DEFAULT_FILTER, stem + '.xml', stem + '.log', {'TW_BENCH_OUT': stem, 'TW_BENCH_SEEDS': seeds})
    if got is None or not os.path.exists(stem + '.json'):
        return None
    return json.load(open(stem + '.json'))


def compare_bench(now, then):
    """Lines for the report, and the metrics that got worse on every seed."""
    lines, worse = [], []
    seeds = [k for k in now if k != 'mean']
    same = then is not None and sorted(seeds) == sorted(k for k in then if k != 'mean')
    for m in now['mean']:
        a = now['mean'][m]
        b = then['mean'].get(m) if then else None
        verdict = ''
        if same and b is not None:
            ups = sum(1 for s in seeds if now[s].get(m, 0) > then[s].get(m, 0))
            downs = sum(1 for s in seeds if now[s].get(m, 0) < then[s].get(m, 0))
            verdict = 'all up' if ups == len(seeds) else 'all down' if downs == len(seeds) else 'same' if ups == downs == 0 else 'mixed'
            if verdict == 'all up' and m in WORSE_UP:
                worse.append(m)
        lines.append(f'| {m} | {a:.3f} | {"" if b is None else f"{b:.3f}"} | {verdict} |')
    return lines, worse


def gym_flags(run):
    got = {}
    for tab in os.listdir(run):
        d = os.path.join(run, tab)
        if not os.path.isdir(d) or tab == 'raw':
            continue
        for f in os.listdir(d):
            if f.endswith('.json'):
                j = json.load(open(os.path.join(d, f)))
                got[j.get('entry', tab + '/' + f[:-5])] = j.get('flags', [])
    return got


def main():
    ap = argparse.ArgumentParser(description='The behaviour bench and the gym, compared with the night before.')
    ap.add_argument('--seeds', default='1,2,3,4,5,6')
    ap.add_argument('--gym', default='minutes=60', help='gym options (every tab when no tabs= is given)')
    ap.add_argument('--keep', type=int, default=7)
    ap.add_argument('--skip-bench', action='store_true')
    ap.add_argument('--skip-gym', action='store_true')
    args = ap.parse_args()
    os.chdir(PROJECT)
    root = night_root()
    sha = git('rev-parse', '--short=8', 'HEAD') or 'nogit'
    dirty = bool(git('status', '--porcelain', '--untracked-files=no'))
    stamp = datetime.datetime.now().strftime('%Y%m%d-%H%M')
    name = f'{stamp}-{sha}'
    night = os.path.join(root, name)
    os.makedirs(night, exist_ok=True)
    report = {'night': name, 'sha': sha, 'dirty': dirty, 'branch': git('rev-parse', '--abbrev-ref', 'HEAD'), 'look': []}
    md = [f'# Night {name}', '', f'Commit {sha} on {report["branch"]}{" (tree dirty)" if dirty else ""}.', '']
    ran = False

    if not args.skip_bench:
        then_dir = previous(root, name, 'bench.json')
        now = bench_part(night, args.seeds)
        md += ['## Behaviour bench', '']
        if now is None:
            md += ['The bench wrote no report (see bench.log).', '']
            report['look'].append('bench did not run')
        else:
            ran = True
            then = json.load(open(os.path.join(then_dir, 'bench.json'))) if then_dir else None
            lines, worse = compare_bench(now, then)
            md += [f'Against {os.path.basename(then_dir) if then_dir else "nothing (the first night)"}.', '',
                   '| metric | tonight | last time | seeds |', '|---|---|---|---|'] + lines + ['']
            for m in worse:
                report['look'].append(f'bench: {m} rose on every seed')
            if os.path.exists(os.path.join(night, 'bench.txt')):
                md += ['```', open(os.path.join(night, 'bench.txt')).read().rstrip(), '```', '']

    if not args.skip_gym:
        then_dir = previous(root, name, os.path.join('gym', 'run', 'summary.json'))
        run = abtest.gym_run(args.gym, night, 'gym')
        md += ['## Gym', '']
        if run is None:
            md += ['The gym wrote no summary (see gym.log).', '']
            report['look'].append('gym did not run')
        else:
            ran = True
            summary = json.load(open(os.path.join(run, 'summary.json')))
            flags = gym_flags(run)
            md += [f'{summary.get("done")} entries, {summary.get("flagged")} flagged, {summary.get("seconds")} s'
                   + (f', stopped: {summary["stopped"]}' if summary.get('stopped') else '') + '.', '']
            then_run = os.path.join(then_dir, 'gym', 'run') if then_dir else None
            then_flags = gym_flags(then_run) if then_run else {}
            new = [(e, f) for e, fs in sorted(flags.items()) for f in fs if f not in then_flags.get(e, [])]
            md += ['New flags:' if new else 'No new flags.'] + [f'- {e}: {f}' for e, f in new] + ['']
            for e, f in new:
                report['look'].append(f'gym: {e}: {f}')
            if then_run:
                then_sha = os.path.basename(then_dir).split('-')[-1]
                base, other = abtest.sheets(then_run), abtest.sheets(run)
                moved = []
                for entry, path in sorted(other.items()):
                    if entry not in base:
                        continue
                    bands = abtest.band_count(path)
                    change = abtest.band_change(base[entry], path, bands)
                    if max(change) > SAME_COMMIT_NOISE:
                        moved.append((entry, change))
                same_commit = then_sha == sha and not dirty
                label = ('a different commit' if then_sha != sha else 'the same commit, the tree dirty' if dirty
                         else 'the same commit: any change is the gym')
                md += [f'Pictures against {os.path.basename(then_dir)} ({label}): '
                       + (f'{len(moved)} of {len(other)} entries changed.' if moved else 'none changed.'), '']
                md += [f'- {e}: ' + ', '.join(f'{c:.3f}' for c in ch) for e, ch in moved[:60]] + ([''] if moved else [])
                if same_commit:
                    for e, _ in moved:
                        report['look'].append(f'gym: {e} changed on the same commit')

    report['ran'] = ran
    md += ['## To look at', ''] + ([f'- {x}' for x in report['look']] or ['Nothing new.']) + ['']
    open(os.path.join(night, 'report.md'), 'w', encoding='utf-8', newline='\n').write('\n'.join(md))
    json.dump(report, open(os.path.join(night, 'report.json'), 'w'), indent=1)
    nights = sorted(d for d in os.listdir(root) if os.path.isdir(os.path.join(root, d)))
    for d in nights[:-args.keep]:
        shutil.rmtree(os.path.join(root, d), ignore_errors=True)
    print('\n'.join(md))
    print(f'nightly: {night}')
    sys.exit(1 if not ran else 2 if report['look'] else 0)


if __name__ == '__main__':
    main()
