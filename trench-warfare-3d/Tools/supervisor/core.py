#!/usr/bin/env python3
"""The supervisor's base: a switch per module (off, dry, on), its state file, its log line, its turn.

WHY. The review of the relays of 2026-10-19 to 11-02 (PLAN_relay_harness_v2_2026-11-02.md on the Drive) found the
loop's jobs in one 1,415-line file (Tools/assetboard/autoloop.py), so one fault stops all of them and a fix can not
be tried alone. The owner asked for "everything modular" (2026-11-02). Here every job is a module: one file in
Tools/supervisor/modules/, one decision, its own switch, its own state file, its own tests.

A module is a Python file in Tools/supervisor/modules/ that defines:

    NAME = 'breaker'          # its name: the switch, the state file and the log lines use it
    EVERY_S = 300             # how often main.py gives it a turn
    DEFAULT_MODE = 'dry'      # off | dry | on, until modules.json in the home says otherwise
    def run(ctx) -> list      # reads files, decides, writes ITS OWN state; returns log lines

`ctx.dry` is True in dry mode: the module decides and logs what it would do, and changes nothing outside its own
state file. A new module edits no shared file: main.py finds it by its folder, and its switch defaults to its own
DEFAULT_MODE. The owner's (or a session's) word on a switch goes in `<home>/modules.json`, e.g. {"breaker": "on"}.

The home is %LOCALAPPDATA%/TrenchWarfare/supervisor, or TW_SUP_HOME (tests set it).
"""
import importlib.util
import json
import os
import time
from pathlib import Path

MODES = ('off', 'dry', 'on')
MODULES_DIR = Path(__file__).resolve().parent / 'modules'


def home():
    h = os.environ.get('TW_SUP_HOME')
    if h:
        return Path(h)
    return Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'supervisor'


def read_json(path, default=None):
    try:
        with open(path, encoding='utf-8') as f:
            return json.load(f)
    except (OSError, ValueError):
        return default


def write_json(path, data):
    """Write whole or not at all: a reader never sees half a file."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + '.tmp')
    with open(tmp, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=1, sort_keys=True)
    os.replace(tmp, path)


def discover(folder=None):
    """Every module in the folder, by file name order. A file that does not load is reported, not raised."""
    found, broken = [], []
    folder = Path(folder or MODULES_DIR)
    for p in sorted(folder.glob('*.py')):
        if p.name.startswith(('_', 'test_')):
            continue
        try:
            spec = importlib.util.spec_from_file_location(f'tw_sup_{p.stem}', p)
            m = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(m)
        except Exception as e:  # a broken module must not stop the others
            broken.append(f'{p.name}: {type(e).__name__}: {e}')
            continue
        if all(hasattr(m, k) for k in ('NAME', 'run')):
            found.append(m)
        else:
            broken.append(f'{p.name}: no NAME or run()')
    return found, broken


def mode(module, h=None):
    """The module's switch: modules.json in the home wins, else the module's own DEFAULT_MODE, else off."""
    over = read_json(Path(h or home()) / 'modules.json', {}) or {}
    m = over.get(module.NAME, getattr(module, 'DEFAULT_MODE', 'off'))
    return m if m in MODES else 'off'


def state_path(name, h=None):
    return Path(h or home()) / 'state' / f'{name}.json'


def read_state(name, h=None, default=None):
    return read_json(state_path(name, h), default if default is not None else {})


def write_state(name, data, h=None):
    write_json(state_path(name, h), data)


def log(name, line, h=None, now=None):
    p = Path(h or home()) / 'supervisor.log'
    p.parent.mkdir(parents=True, exist_ok=True)
    stamp = time.strftime('%m-%d %H:%M:%S', time.localtime(now or time.time()))
    with open(p, 'a', encoding='utf-8') as f:
        f.write(f'{stamp} {name}: {line}\n')


class Ctx:
    """What a module gets on its turn."""

    def __init__(self, h, now, dry, name):
        self.home, self.now, self.dry, self.name = Path(h), now, dry, name

    def state(self, default=None):
        return read_state(self.name, self.home, default)

    def save(self, data):
        write_state(self.name, data, self.home)

    def other(self, name, default=None):
        """Another module's state, read-only: the one way modules hear each other."""
        return read_state(name, self.home, default)


def due(module, h=None, now=None):
    last = read_json(Path(h or home()) / 'state' / '_turns.json', {}) or {}
    return (now or time.time()) - last.get(module.NAME, 0) >= getattr(module, 'EVERY_S', 300)


def turn(module, h=None, now=None):
    """Give one module its turn under its switch. Returns its log lines (prefixed [dry] in dry mode)."""
    h, now = Path(h or home()), now or time.time()
    m = mode(module, h)
    if m == 'off':
        return []
    ctx = Ctx(h, now, m == 'dry', module.NAME)
    try:
        lines = list(module.run(ctx) or [])
    except Exception as e:
        lines = [f'FAILED: {type(e).__name__}: {e}']
    lines = [('[dry] ' if ctx.dry else '') + str(l) for l in lines]
    for l in lines:
        log(module.NAME, l, h, now)
    turns = read_json(h / 'state' / '_turns.json', {}) or {}
    turns[module.NAME] = now
    write_json(h / 'state' / '_turns.json', turns)
    return lines
