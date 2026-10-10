#!/usr/bin/env python3
"""Tests of what a leg may not leave behind (launch.leftover_unity). Run from trench-warfare-3d/:
python Tools/relay/test_leftover.py

The command lines are the desktop's of the evening of 2026-10-10, when a leg's film capture held the work checkout
for six hours after the leg was cut. Nothing is started or stopped here."""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import launch    # noqa: E402

results = []
U = r'"C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe"'
WORK = r'C:\Users\PC\Documents\GitHub\githubtest-relay-work'
LEG = 1000000          # when the leg began


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:600]))


def main():
    film = (64552, LEG + 2040, U + r' -batchmode -projectPath ' + WORK + r'\trench-warfare-3d -logFile C:\Users\PC\x\Logs\balloonfilm-editor.log')
    other = (55696, LEG + 60, U + r' -batchmode -projectPath ' + WORK + r'2\trench-warfare-3d -logFile C:\x\Tools\manshots.log')
    window = (47388, LEG + 60, U + ' -projectPath "' + WORK + r'\trench-warfare-3d"')
    bare = (47389, LEG + 60, U + '  cmd')
    before = (500, LEG - 600, U + r' -batchmode -projectPath ' + WORK + r'\trench-warfare-3d -runTests')
    quoted = (501, LEG + 5, U + ' -batchmode -nographics -projectPath "' + WORK.replace('\\', '/') + '/trench-warfare-3d" -executeMethod X.Y')
    got = launch.leftover_unity([film, other, window, bare, before, quoted], WORK, LEG)
    case('a batch-mode Unity the leg started on its own checkout is a leftover, however its path is written',
         got == [64552, 501], got)
    case('never a leftover: a Unity on another checkout (relay-work2), an editor window, one with no project, one that was there before the leg',
         not {55696, 47388, 47389, 500} & set(got), got)
    case('a leg that works in the project folder itself is matched too, and no Unity at all is no leftover',
         launch.leftover_unity([film], WORK + r'\trench-warfare-3d', LEG) == [64552] and launch.leftover_unity([], WORK, LEG) == [])
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
