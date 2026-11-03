#!/usr/bin/env python3
"""The Health page (src_health.py, templates/health.html, static/health.js). Run from trench-warfare-3d/:
python Tools/assetboard/test_health_page.py

A supervisor home in a temp folder holds the state files the modules write (breaker open, a budget window, capacity,
a family over its cap, a critic hold, a landing gate, landed.json, a red canary, switches and turns); the page's data
must say each of them, an empty home must read as "no record" everywhere without failing, and the page's own
script (under node, when this machine has it) must turn the reading into its cards."""
import json
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import src_health  # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:900]))


def put(p, d):
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(d), encoding='utf-8')


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-health-test-'))
    try:
        now = time.mktime((2026, 11, 3, 10, 0, 0, 0, 0, -1))
        h, loop = tmp / 'sup', tmp / 'loop'
        mods = tmp / 'modules'
        mods.mkdir(parents=True)
        (mods / 'breaker.py').write_text("NAME = 'breaker'\nEVERY_S = 300\nDEFAULT_MODE = 'dry'\n", encoding='utf-8')
        (mods / 'loop_01_back.py').write_text("NAME = 'loop_back'\nEVERY_S = 30\nDEFAULT_MODE = 'off'\n", encoding='utf-8')
        (mods / 'test_x.py').write_text("NAME = 'not_a_module'\n", encoding='utf-8')
        put(h / 'modules.json', {'loop_back': 'on'})
        put(h / 'state' / '_turns.json', {'breaker': now - 60})
        put(h / 'state' / 'breaker.json', dict(open=True, kind='licence', since=now - 600, reason='3 different units ended on the same machine fault (licence) within 60 min',
                                               evidence=['a (relay 2): no licence', 'b (relay 3): no licence']))
        put(h / 'state' / 'budget.json', dict(standing=11.0, windows=[dict(kind='more', pct=7, start='2026-11-03T08:00:00', end='2026-11-03T18:00:00'),
                                                                      dict(kind='day', pct=26, start='2026-11-02T00:00:00', end='2026-11-03T00:00:00')]))
        put(h / 'state' / 'capacity.json', dict(allowed=2, why='3.10% left of 11% for 8.0 h', cap=11, spent=7.9, hours=8.0, at=now - 30))
        put(h / 'state' / 'families.json', dict(at=now, cap_usd=100, cap_generations=3, families={
            'goal-z': dict(over='$120 spent (cap $100)', usd=120, depth=1, brief='2026-11-02-goal-z'), 'goal-y': dict(over='', usd=4, depth=1)}))
        put(h / 'state' / 'critic_gate.json', dict(held={'smoke-look': dict(score=55, fixes=['x'], at='2026-11-03T09:10:00')}, passed=['a', 'b']))
        put(h / 'state' / 'landing.json', dict(phase='gating', gate=dict(started=now - 900)))
        put(h / 'landed.json', dict(weeks={time.strftime('%G-W%V', time.localtime(now)): 3}, last_landed_at='2026-11-03T08:30:00'))
        put(h / 'state' / 'canary.json', dict(verdict='red', kind='licence', at=now - 1200, line='no licence'))
        loop.mkdir()
        (loop / 'autoloop.stop').write_text('owner: pause', encoding='utf-8')

        H = src_health.read(h, now, loop_home=loop, folder=mods)
        names = {m['name']: m for m in H['modules']}
        case('every module file is listed with its switch (modules.json over its default) and last turn; a test file is not',
             set(names) == {'breaker', 'loop_back'} and names['loop_back']['mode'] == 'on' and names['loop_back']['set']
             and names['breaker']['mode'] == 'dry' and names['breaker']['last'] and names['loop_back']['last'] == '', H['modules'])
        b = H['breaker'] or {}
        case('the breaker: open, its kind, why and its evidence', b.get('open') and b.get('kind') == 'licence'
             and 'machine fault' in str(b.get('reason')) and len(b.get('evidence') or []) == 2, b)
        case('the budget: the window that holds now (not the newest written, not one that has ended), and capacity',
             H['budget']['pct'] == 7 and H['budget']['kind'] == 'more' and H['budget']['end'] == '2026-11-03 18:00'
             and H['capacity']['allowed'] == 2, (H['budget'], H['capacity']))
        case('families over cap: only those with an over line', [f['family'] for f in H['families']['over']] == ['goal-z'], H['families'])
        case('critic holds, landing phase and "landed this week", the last canary, and the pause',
             [x['unit'] for x in H['critic']['held']] == ['smoke-look'] and H['critic']['passed'] == 2
             and H['landing']['phase'] == 'gating' and H['landing']['landed']['week'] == 3
             and H['canary']['verdict'] == 'red' and H['paused'] == dict(loop=True, supervisor=False), H)

        E = src_health.read(tmp / 'empty', now, loop_home=tmp / 'none', folder=mods)
        t = src_health.text(E)
        case('an empty home reads without failing: no record for each subject, the standing 11% for the day',
             E['breaker'] is None and E['capacity'] is None and E['families'] is None and E['critic'] is None
             and E['canary'] is None and E['budget']['pct'] == 11.0 and E['landing']['phase'] == 'no record'
             and 'Breaker: no record' in t, (E, t))

        out = tmp / 'site'
        src_health.page(out)
        js = src_health.site(H, out)
        page = (out / 'health.html').read_text(encoding='utf-8')
        data = js.read_text(encoding='utf-8')
        case('the page is written with its data: health.html loads data/health.js and health.js; the data is the reading',
             'src="data/health.js"' in page and 'src="health.js"' in page and (out / 'health.js').exists()
             and json.loads(data[len('window.HEALTH = '):].rstrip().rstrip(';')) == json.loads(json.dumps(H)), page[-400:])

        node = shutil.which('node')
        if node:
            probe = ("const m = require(process.argv[1]); const H = JSON.parse(require('fs').readFileSync(process.argv[2], 'utf8')"
                     ".slice('window.HEALTH = '.length).trim().replace(/;$/, '')); console.log(JSON.stringify([m.healthCards(H), m.healthCards(null)]));")
            r = subprocess.run([node, '-e', probe, str(out / 'health.js'), str(js)], capture_output=True, text=True, timeout=60)
            try:
                cards, none = json.loads(r.stdout)
            except ValueError:
                cards, none = [], []
            titles = [c['title'] for c in cards]
            case('the page\'s script (node): a card each, the open breaker and the pause first in trouble colours, the hold '
                 'and the family named; no reading is one card that says so',
                 titles[:2] == ['Paused', 'Breaker OPEN (licence)'] and cards[1]['tone'] == 'bad'
                 and any('goal-z' in l for c in cards for l in c['lines']) and any('smoke-look: 55' in l for c in cards for l in c['lines'])
                 and any(c['title'] == 'Modules' and any(l.startswith('loop_back: on,') for l in c['lines']) for c in cards)
                 and len(none) == 1 and none[0]['tone'] == 'bad', (r.stdout[:600], r.stderr[:400]))
        else:
            print('skip  node is not on this machine: the page\'s script is not run')
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'\n{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
