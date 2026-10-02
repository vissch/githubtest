"""What git says about the assets: every commit that changed an asset's files or named it, and the lanes in flight.

Other branches are read where they are (git log / diff against the integration branch): nothing is checked out and
nothing is fetched. The footer of the site says how old the remote refs are.

A lane is LIVE when it has commits integration lacks (patch-equivalent ones do not count), it is the newest of its
family (a -v2, -v3 or -land copy supersedes the older name), and it is either checked out in a worktree on this
machine or its tip is at most LIVE_DAYS old. Everything else with unlanded commits is PARKED: shown, greyed, and
never the reason an asset counts as in progress.
"""
import datetime
import re
import subprocess
from pathlib import Path

import model

INTEGRATION = 'origin/claude/trench-warfare-2d-3d-plan-idt7lf'
PREFIX = 'trench-warfare-3d/Assets/_Project/'
LIVE_DAYS = 7
# model folders: a name here that integration does not know is an asset that exists only on a lane
NEW_ASSET = [(re.compile(PREFIX + r'Resources/Vehicles/([A-Z]\w+)/'), 'vehicle'),
             (re.compile(PREFIX + r'Playground/Art/Tanks/([A-Z]\w+)/'), 'vehicle'),
             (re.compile(PREFIX + r'Playground/Art/Units/([A-Z]\w+)/'), 'character')]


def git(repo, *args):
    p = subprocess.run(['git', '-C', str(repo), *args], capture_output=True)
    return p.stdout.decode('utf-8', 'replace')


def head(repo):
    commit = git(repo, 'rev-parse', '--short', INTEGRATION).strip()
    branch = git(repo, 'rev-parse', '--abbrev-ref', 'HEAD').strip()
    common = Path(git(repo, 'rev-parse', '--path-format=absolute', '--git-common-dir').strip())
    fetched = common / 'FETCH_HEAD'
    as_of = datetime.datetime.fromtimestamp(fetched.stat().st_mtime).strftime('%Y-%m-%d %H:%M') if fetched.exists() else 'unknown'
    return commit, branch, as_of


def commits(repo, rev_range):
    """[(sha, date, subject, [files])] newest first."""
    out, cur = [], None
    for line in git(repo, 'log', rev_range, '--date=short', '--format=@@%h|%ad|%s', '--name-only').split('\n'):
        if line.startswith('@@'):
            sha, date, subject = line[2:].split('|', 2)
            cur = (sha, date, subject, [])
            out.append(cur)
        elif line.strip() and cur:
            cur[3].append(line.strip())
    return out


def matcher(a):
    rx = re.compile(a['mention'], re.I if a['category'] == 'character' else 0) if a.get('mention') else None
    roots = tuple(PREFIX + r for r in a['roots'])
    return rx, roots


def family(name):
    base = re.sub(r'-land\d*$', '', name)
    base = re.sub(r'-v\d+$', '', base)
    if not re.search(r'\d{4}-\d{2}-\d{2}$', base):
        base = re.sub(r'-?\d$', '', base)
    return base


def version(name):
    """How late a copy of a lane this name is: -v3 beats -v2 beats the bare name; a -land copy is the landed one."""
    m = re.search(r'-v(\d+)$', re.sub(r'-land\d*$', '', name))
    return int(m.group(1)) if m else 1


def lanes(repo, today=None):
    """Every branch with commits integration lacks: name, tip date, ahead, worktree, live or parked, and its commits."""
    today = today or datetime.date.today()
    worktrees, cur = {}, None
    for line in git(repo, 'worktree', 'list', '--porcelain').split('\n'):
        if line.startswith('worktree '):
            cur = Path(line[9:]).name
        elif line.startswith('branch refs/heads/'):
            worktrees[line[18:]] = cur
    refs = {}
    for scope in ('refs/remotes/origin', 'refs/heads'):
        for line in git(repo, 'for-each-ref', '--format=%(refname:short)|%(committerdate:short)', scope).split('\n'):
            if '|' not in line:
                continue
            ref, date = line.split('|')
            name = ref[7:] if scope.endswith('origin') and ref.startswith('origin/') else ref
            if name in ('origin', 'HEAD', 'main', INTEGRATION[7:]) or not name:
                continue
            refs[name] = (ref, date)            # a local branch (possibly ahead of origin) wins over the remote one
    out = []
    for name, (ref, date) in sorted(refs.items()):
        ahead = git(repo, 'rev-list', '--count', '--cherry-pick', '--right-only', f'{INTEGRATION}...{ref}').strip()
        if not ahead or int(ahead) == 0:
            continue
        out.append(dict(branch=name, ref=ref, tip=date, ahead=int(ahead), worktree=worktrees.get(name), family=family(name)))
    newest = {}
    for l in out:
        best = newest.get(l['family'])
        if best is None or (version(l['branch']), l['tip'], l['ahead']) > (version(best['branch']), best['tip'], best['ahead']):
            newest[l['family']] = l
    for l in out:
        age = (today - datetime.date.fromisoformat(l['tip'])).days
        l['newest'] = newest[l['family']] is l
        l['live'] = l['newest'] and (l['worktree'] is not None or age <= LIVE_DAYS)
        l['commits'] = commits(repo, f'{INTEGRATION}..{l["ref"]}') if l['newest'] else []
        l['assets'] = []
    return out


def attach(repo, assets, warnings):
    """Fill each asset's history and lanes. Returns the lanes for the process page."""
    if not git(repo, 'rev-parse', '--verify', '--quiet', INTEGRATION).strip():
        warnings.append(f'git: no {INTEGRATION} ref here, so there is no history and no lanes on this build')
        return []
    match = {aid: matcher(a) for aid, a in assets.items()}

    def hits(subject, files, aid):
        rx, roots = match[aid]
        art = [f[len(PREFIX):] for f in files if roots and f.startswith(roots) and not f.endswith('.meta')]
        return art, bool(rx and rx.search(subject))

    for sha, date, subject, files in commits(repo, INTEGRATION):
        for aid, a in assets.items():
            art, named = hits(subject, files, aid)
            if art or named:
                a['history'].append(dict(date=date, kind='commit' if art else 'mention', sha=sha, text=subject, files=art[:6], lane=None))

    all_lanes = lanes(repo)
    for l in all_lanes:
        if l['live']:                     # a model folder integration has never heard of: an asset that is only on this lane
            for _, _, _, files in l['commits']:
                for f in files:
                    for rx, category in NEW_ASSET:
                        m = rx.match(f)
                        if m and m.group(1) not in assets:
                            assets[m.group(1)] = model.lane_only_asset(m.group(1), category, l['branch'])
                            match[m.group(1)] = matcher(assets[m.group(1)])
    for l in all_lanes:
        for aid, a in assets.items():
            art_files, subjects, seen = [], [], set()
            for sha, date, subject, files in l['commits']:
                art, named = hits(subject, files, aid)
                if not (art or named):
                    continue
                art_files += [f for f in art if f not in art_files]
                subjects.append(subject)
                key = (subject, date)
                if key not in seen:
                    seen.add(key)
                    a['history'].append(dict(date=date, kind='lane' if art else 'lane-mention', sha=sha, text=subject, files=art[:6], lane=l['branch']))
            if art_files or subjects:
                a['lanes'].append(dict(branch=l['branch'], live=l['live'], worktree=l['worktree'], ahead=l['ahead'], tip=l['tip'],
                                       touches_art=bool(art_files), files=art_files[:8], subjects=subjects[:5]))
                l['assets'].append(dict(id=aid, touches_art=bool(art_files)))
    for a in assets.values():
        a['history'].sort(key=lambda h: (h['date'], h['kind'] != 'commit'), reverse=True)
        a['lanes'].sort(key=lambda l: (not l['live'], not l['touches_art'], l['branch']))
    for l in all_lanes:
        l['commits'] = len(l['commits'])
    return all_lanes
