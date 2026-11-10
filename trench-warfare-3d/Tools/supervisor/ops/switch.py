#!/usr/bin/env python3
"""Switch a supervisor module: python Tools/supervisor/ops/switch.py <module> <off|dry|on>
                                python Tools/supervisor/ops/switch.py            (list every module and its switch)

Writes <home>/modules.json whole (core.write_json) and one line in supervisor.log saying who switched what, so a
switch is never a hand edit nobody can trace. The supervisor reads the file again every turn: no restart.
A name that is no module is refused (a typo would silently be no switch at all)."""
import getpass
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402


def names():
    found, _ = core.discover()
    return {m.NAME: m for m in found}


def show(h):
    over = core.read_json(Path(h) / 'modules.json', {}) or {}
    for name, m in sorted(names().items()):
        mode = core.mode(m, h)
        print('%-18s %-4s%s' % (name, mode, '' if name in over else '   (its default)'))


def switch(name, mode, h=None, who=None):
    h = Path(h or core.home())
    mods = names()
    if name not in mods:
        raise SystemExit('switch: no module %r; modules: %s' % (name, ', '.join(sorted(mods))))
    if mode not in core.MODES:
        raise SystemExit('switch: mode is one of %s' % ', '.join(core.MODES))
    f = h / 'modules.json'
    over = core.read_json(f, {}) or {}
    if not isinstance(over, dict):
        raise SystemExit('switch: %s does not read as an object: mend it by hand first' % f)
    was = core.mode(mods[name], h)
    over[name] = mode
    core.write_json(f, over)
    line = '%s switch: %s %s -> %s (by %s)' % (time.strftime('%m-%d %H:%M:%S'), name, was, mode, who or getpass.getuser())
    with open(h / 'supervisor.log', 'a', encoding='utf-8') as log:
        log.write(line + '\n')
    return line


if __name__ == '__main__':
    if len(sys.argv) == 1:
        show(core.home())
    elif len(sys.argv) == 3:
        print(switch(sys.argv[1], sys.argv[2]))
    else:
        raise SystemExit(__doc__)
