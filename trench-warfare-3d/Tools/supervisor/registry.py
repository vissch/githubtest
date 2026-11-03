#!/usr/bin/env python3
"""One answer to "is unit X done, and where?" for the relays, the loop, the Grok worker and sessions.

    python Tools/supervisor/registry.py is <uid> [--lane <its lane>]     done or not, and where (exit 0 = done)
    python Tools/supervisor/registry.py record <uid> --lane <lane> --sha <sha> [--by grok]
                                                    a unit finished outside the relay: kept in the registry's own
                                                    file (state/registry_records.json), never on a board
    python Tools/supervisor/registry.py refresh [--repo <checkout>]    rebuild the cache now (the module does it
                                                    every EVERY_S under its switch: modules/registry_refresh.py)

    from registry import is_done
    done, where = is_done('fix-job-ordering-tests', lane='lane/show/review-tools')

WHY. From 10-19 to 11-02 the relays worked 28 units again that the Grok worker had already built and pushed on
lanes named lane/show/grok-<unit id> (133 legs, about $118): the picker only asked <board>/relay/done/<id>.json, and
nothing wrote that record for work done outside the relay. Here "done" has four sources, read-only:

  board        <board>/relay/done/<id>.json on the main board (tw3d-board) or an extra one (tw3d-board-N)
  integration  a commit on origin/<integration> whose subject STARTS with the tag [<id>] (the plan's source; a
               landed lane can carry a leg's half-done [<id>] too, but it landed through the full gate and the
               owner's word, so its unit is not queued again blind: its board record or a new id says otherwise)
  lane         a commit on lane/show/grok-<id> (not yet on integration) whose subject starts with [<id>]: the
               Grok worker pushes a unit only once its test passes. Only that lane's own id counts: a second tag
               on the commit, or a [Y] the lane inherited from the relay lane it was cut from, does not (a unit
               finished on another unit's Grok lane is written with `record`). A tag on any OTHER lane is not "done": a relay
               leg pushes its unit's half-done work under the unit's tag on the unit's own lane before the next leg
               goes on (the board's done record is what says its done_when passed), and a review lane carries the
               tags of what it looks at. So only the Grok lanes are read (36 of ~630 origin lanes on 11-02)
  record       `registry.py record`, for work finished outside the relay that carries no tag

A tag counts only in the run of [..] tags that opens the subject: "[sup-core] fix the [old-id] check" says nothing
about old-id, and "Revert \"[x] ...\"" does not open with a tag.

The cache (state/registry_refresh.json) is refreshed from `git for-each-ref` and `git log --format=%s` over the
commits of each Grok lane that moved since the last refresh (only origin/<integration>..<lane>) and integration's
new commits, so a refresh after the first is a handful of git calls. It does not fetch: the relays and the loop fetch the checkout it reads.
"""
import argparse
import os
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
if str(HERE) not in sys.path:
    sys.path.append(str(HERE))
import core  # noqa: E402

STATE = 'registry_refresh'          # the module's state: the cache
RECORDS = 'registry_records'        # what `record` writes; the refresh never rewrites it
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'     # found.INTEGRATION
GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
REPO = Path(os.environ.get('TW_SUP_REPO') or HERE.parents[2])       # the checkout this file runs from
GROK = 'lane/show/grok-'
LEAD = re.compile(r'^\s*((?:\[[^\[\]\s]+\]\s*)+)')
TAG = re.compile(r'\[([^\[\]\s]+)\]')


def tags(subject):
    """The unit ids a commit subject is tagged with: only the [..] run that opens it."""
    m = LEAD.match(subject or '')
    return set(TAG.findall(m.group(1))) if m else set()


def boards(gh=None):
    """{name: folder} of the main board and the extra boards that exist."""
    gh = Path(gh or GH)
    out = {}
    for p in sorted(gh.glob('tw3d-board*')):
        if re.fullmatch(r'tw3d-board(-\d+)?', p.name) and (p / 'relay').is_dir():
            out[p.name] = p
    return out


def board_done(bds):
    """{uid: 'board:<name>'} from the done records; the main board's name wins when two have it."""
    out = {}
    for name, b in sorted(bds.items(), key=lambda kv: (kv[0] != 'tw3d-board', kv[0])):
        d = Path(b) / 'relay' / 'done'
        for f in (d.glob('*.json') if d.is_dir() else []):
            out.setdefault(f.stem, f'board:{name}')
    return out


def git(repo, *a):
    r = subprocess.run(['git', '-C', str(repo), *a], capture_output=True, stdin=subprocess.DEVNULL, timeout=600)
    if r.returncode:
        raise RuntimeError(f'git {a[0]} failed: {r.stderr.decode("utf-8", "replace").strip()[:200]}')
    return r.stdout.decode('utf-8', 'replace')


def refresh(state, repo=None, bds=None, gitf=git):
    """The new cache from the old one: (state, [log lines]). Only lanes whose head moved are read again."""
    repo = repo or REPO
    state = dict(state or {})
    old = state.get('refs') or {}
    lanes = dict(state.get('lanes') or {})          # {lane: {tag: sha of the newest commit carrying it}}
    integ = dict(state.get('integration') or {})    # {tag: sha}
    refs = {}
    for line in gitf(repo, 'for-each-ref', '--format=%(refname:short)%09%(objectname)', 'refs/remotes/origin').splitlines():
        name, _, sha = line.partition('\t')
        name = name[len('origin/'):] if name.startswith('origin/') else name
        if name == INTEGRATION or name.startswith(GROK):
            refs[name] = sha
    ib = refs.get(INTEGRATION)
    read = 0
    if ib and ib != old.get(INTEGRATION):
        span = f'{old[INTEGRATION]}..{ib}' if old.get(INTEGRATION) else ib
        try:
            text = gitf(repo, 'log', '--format=%H%x09%s', span)
        except RuntimeError:            # the old head is gone (a force-push): read it all again
            text, integ = gitf(repo, 'log', '--format=%H%x09%s', ib), {}
        for line in reversed(text.splitlines()):
            sha, _, subj = line.partition('\t')
            for t in tags(subj):
                integ[t] = sha
        read += 1
    for lane, sha in refs.items():
        if lane == INTEGRATION or (old.get(lane) == sha and lane in lanes):
            continue                    # unmoved; a tag of it that has landed since is on integration's list as well
        got, own = {}, lane[len(GROK):]
        text = gitf(repo, 'log', '--format=%H%x09%s', sha, *([f'^{ib}'] if ib else []))
        for line in reversed(text.splitlines()):
            c, _, subj = line.partition('\t')
            if own in tags(subj):       # only the lane's own unit: a Grok lane cut from a relay lane inherits
                got[own] = c            # that lane's half-done [Y] commits, and a second tag rides along unchecked

        lanes[lane] = got
        read += 1
    for lane in [l for l in lanes if l not in refs]:
        lanes.pop(lane)
    state.update(refs=refs, lanes=lanes, integration=integ, boards=board_done(bds if bds is not None else boards()))
    n = sum(1 for v in lanes.values() if v)
    return state, [f'{read} refs read again of {len(refs)}; {len(integ)} tags on integration, {n} lanes carry tags, '
                   f'{len(state["boards"])} done records on the boards']


def where_done(uid, lane=None, cache=None, records=None):
    """'' when not done, else where: 'board:tw3d-board', 'integration:<sha>', 'lane:<grok lane>@<sha>',
    'record:<lane>@<sha>'. `lane` (the unit's own) is taken for the callers' sake and on purpose counts for nothing:
    its own lane's tag is a leg's handoff, not a pass."""
    cache = cache or {}
    if uid in (cache.get('boards') or {}):
        return cache['boards'][uid]
    if uid in (cache.get('integration') or {}):
        return f'integration:{cache["integration"][uid][:10]}'
    for l, ts in sorted((cache.get('lanes') or {}).items()):
        if uid in ts and l.startswith(GROK):
            return f'lane:{l}@{ts[uid][:10]}'
    r = (records or {}).get(uid)
    if r:
        return f'record:{r.get("lane", "")}@{str(r.get("sha", ""))[:10]}'
    return ''


_MEMO = {}


def _load(h=None):
    """The cache and the records, read once per change of their files (the picker asks for every queued unit)."""
    h = Path(h or core.home())
    out = []
    for name in (STATE, RECORDS):
        p = core.state_path(name, h)
        try:
            key = (str(p), p.stat().st_mtime_ns)
        except OSError:
            out.append({})
            continue
        if _MEMO.get(name, (None,))[0] != key:
            _MEMO[name] = (key, core.read_json(p, {}) or {})
        out.append(_MEMO[name][1])
    return out


def is_done(uid, lane=None, h=None):
    """(True, where) when the registry knows the unit done, else (False, '')."""
    cache, records = _load(h)
    w = where_done(uid, lane, cache, records)
    return bool(w), w


def done_of(units, h=None):
    """{uid: where} of the units {uid: unit} the registry calls done (each judged with its own lane)."""
    cache, records = _load(h)
    out = {}
    for uid, u in (units or {}).items():
        w = where_done(uid, (u or {}).get('lane'), cache, records)
        if w:
            out[uid] = w
    return out


def record(uid, lane, sha, by='', h=None, now=None):
    if not re.fullmatch(r'[\w.-]+', uid or ''):
        raise ValueError(f'{uid!r} is no unit id')
    if not re.fullmatch(r'[0-9a-f]{7,40}', sha or ''):
        raise ValueError(f'{sha!r} is no commit sha')
    recs = core.read_state(RECORDS, h)
    recs[uid] = dict(lane=lane, sha=sha, by=by, at=now or __import__('time').time())
    core.write_state(RECORDS, recs, h)
    return recs[uid]


def main(argv=None):
    ap = argparse.ArgumentParser(description='is a unit done, and where')
    sub = ap.add_subparsers(dest='cmd', required=True)
    a1 = sub.add_parser('is')
    a1.add_argument('uid')
    a1.add_argument('--lane')
    a2 = sub.add_parser('record')
    a2.add_argument('uid')
    a2.add_argument('--lane', required=True)
    a2.add_argument('--sha', required=True)
    a2.add_argument('--by', default='')
    a3 = sub.add_parser('refresh')
    a3.add_argument('--repo')
    a = ap.parse_args(argv)
    if a.cmd == 'is':
        done, w = is_done(a.uid, a.lane)
        print(f'{a.uid}: done ({w})' if done else f'{a.uid}: not done')
        return 0 if done else 1
    if a.cmd == 'record':
        try:
            record(a.uid, a.lane, a.sha, a.by)
        except ValueError as e:
            print(f'registry: {e}')
            return 2
        print(f'{a.uid}: recorded done on {a.lane} at {a.sha}')
        return 0
    st, lines = refresh(core.read_state(STATE), Path(a.repo) if a.repo else None)
    core.write_state(STATE, st)
    print('\n'.join(lines))
    return 0


if __name__ == '__main__':
    sys.exit(main())
