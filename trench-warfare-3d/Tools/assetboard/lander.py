#!/usr/bin/env python3
"""Land a lane when the owner clicks "Land it" on its brief: no agent lands, and no landing is asked for twice.

    python Tools/assetboard/lander.py                 what his clicks would land here, and what was refused (lands nothing)
    python Tools/assetboard/lander.py once            land what he clicked and this station holds
    python Tools/assetboard/lander.py --watch 60      the same, every 60 seconds, until stopped

WHY. The owner, 2026-10-09: "the decisions ive already made and replied on ... keep popping up as if i havent given
any feedback." Most were landings. He had said "land it"; no session may run land.py; so every session wrote a new
brief that asked him to type the command, seven in one day. Asked how a landing should happen after his yes:
"Desktop lands on my click". So an option of a brief can say that it lands a checkout (briefs.py then --land), and
this script, which is no agent, runs Tools/land.py in that checkout when his click is a yes to that line.

What counts as his click is briefs.py answers() and nothing else: go 'land', which is every open note of his about
the brief being a click on the page on that one option, with the stamp of the Then line as it is now. Words of his
own, a second option, a Then line changed after the click: not landed, a session reads them.
land.py decides whether it lands: its checks are not repeated or loosened here. Landed (exit 0): the brief is closed
with where integration is now, and his notes are answered. Refused or failed: the brief stays his answer in progress
and carries `landing` (when, the checkout's HEAD, the note, land.py's last words), which the page shows him under his
answer; the same commit is not tried again for the same note, so a refusal is said once, and a rebase or a new gate
makes a new HEAD that is tried. A checkout this station does not hold is another station's to land: passed over.
"""
import argparse
import datetime
import json
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs  # noqa: E402
import notes   # noqa: E402

LONGEST = 3600          # seconds one landing may take: land.py runs toolcheck when the lane changes a tool
SAID = 400              # characters of land.py's last words kept on the brief


def run_land(tree: Path, land):
    """Run land.py in the checkout: (exit code, what it printed). The lane the checkout is on is checked first: his
    yes was to that lane, and a checkout that has since moved to another is not what he clicked."""
    def git(*a):
        p = subprocess.run(['git', *a], cwd=tree, capture_output=True)
        return (p.stdout + p.stderr).decode('utf-8', 'replace').strip()
    on = git('rev-parse', '--abbrev-ref', 'HEAD')
    if on != land['lane']:
        return 1, f'REFUSED: the checkout is on {on}, not on {land["lane"]}'
    cmd = [sys.executable, 'Tools/land.py'] + (['--carry-sim', land['carry_sim']] if land.get('carry_sim') else [])
    try:
        p = subprocess.run(cmd, cwd=tree, capture_output=True, timeout=LONGEST)
    except subprocess.TimeoutExpired:
        return 1, f'land.py did not end within {LONGEST // 60} minutes; it was stopped and nothing is known to have moved'
    return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace').strip()


def head_of(tree: Path):
    p = subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=tree, capture_output=True)
    return p.stdout.decode('utf-8', 'replace').strip()


def once(where: Path, notes_where: Path, write=True, run=run_land, head=head_of, now=None, by='lander.py'):
    """Land what he clicked. Returns [(brief id, 'landed' | 'refused' | 'tried' | 'would' | 'elsewhere', words)]:
    `tried` is a refusal already said for this commit and this note, `elsewhere` a checkout this station does not hold."""
    out = []
    for a in briefs.answers(briefs.read_all(where), notes.read_all(notes_where)):
        if a['go'] != 'land':
            continue
        land, tree = a['land'], Path(a['land']['checkout'])
        if not (tree / 'Tools' / 'land.py').is_file():
            out.append((a['id'], 'elsewhere', f'{tree} is not on this station'))
            continue
        b, at = briefs.find(where, a['id']), head(tree)
        last = b.get('landing') or {}
        if last.get('head') == at and last.get('note') == a['note']:
            out.append((a['id'], 'tried', last.get('said', '')))
            continue
        if not write:
            out.append((a['id'], 'would', f'land {land["lane"]} from {tree}'))
            continue
        code, said = run(tree, land)
        words = ' '.join(said.split('\n')[-1].split()) if code == 0 else ' '.join(said.split())
        when = now or datetime.datetime.now()
        if code == 0:
            briefs.take(where, notes_where, a['id'], by=by, now=when, note=a['note'], outcome=f'Landed on your click. {words[:SAID]}')
            out.append((a['id'], 'landed', words))
        else:
            b['landing'] = dict(when=f'{when:%Y-%m-%d %H:%M}', head=at, note=a['note'], said=words[:SAID])
            (where / b['id'] / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
            out.append((a['id'], 'refused', words))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='land a lane when the owner clicks Land it on its brief')
    ap.add_argument('what', nargs='?', default='look', choices=('look', 'once'))
    ap.add_argument('--watch', type=int, default=0, metavar='SECONDS', help='land what he clicks, looking every so many seconds')
    a = ap.parse_args(argv)
    while True:
        try:
            got = once(briefs.folder(), notes.folder(), write=bool(a.watch) or a.what == 'once')
        except (OSError, ValueError) as e:           # the Drive away for a moment, a brief half written: the next look reads it
            got = [('lander', 'refused', str(e))]
        for bid, what, words in got:
            if not a.watch or what in ('landed', 'refused'):
                print(f'{datetime.datetime.now():%Y-%m-%d %H:%M:%S}  {what}  {bid}: {words[:300]}', flush=True)
        if not a.watch:
            if not got:
                print('lander: no click of his waits on a landing')
            return 0
        time.sleep(max(10, a.watch))


if __name__ == '__main__':
    sys.exit(main())
