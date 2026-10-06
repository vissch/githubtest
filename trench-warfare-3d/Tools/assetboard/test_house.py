#!/usr/bin/env python3
"""Tests of the house page's rules (static/house.js). Run from trench-warfare-3d/: python Tools/assetboard/test_house.py

house.js is tables and pure functions with no page in it, so node runs it here: the plan (every spot can be walked to,
along the two axes only, through the gaps in the walls and round the furniture), who is put in which room, who gets
which spot, and the frogs over time (they walk when their work changes, come in and leave by the gate, and nobody is
ever left standing). The picture itself (housedraw.js) is looked at, not tested: house.html?demo.
"""
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
results = []


def case(name, ok, detail=''):
    results.append(ok)
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:600]))


HARNESS = r'''
const H = require(process.argv[2]);
const out = {};
const spots = []; for (const r of Object.keys(H.SPOTS)) H.SPOTS[r].forEach((s, i) => spots.push([r, i, s]));
const legs = p => p.slice(1).map((n, k) => [H.NODES[p[k]], H.NODES[n]]);

// the plan
out.unreachable = []; out.diagonal = []; out.matEntry = [];
for (const [r, i, s] of spots) {
  const p = H.route(H.GATE_NODE, s.n);
  if (!p) { out.unreachable.push(r + i); continue; }
  for (const [a, b] of legs(p)) if (a.u !== b.u && a.v !== b.v) out.diagonal.push(r + i);
  if (s.bed) { const [a, b] = legs(p).pop(); if (!(b.u > a.u && b.v === a.v)) out.matEntry.push(r + i); }
}
out.apart = []; let far = 0;
for (const a of spots) for (const b of spots) { const p = H.route(a[2].n, b[2].n); if (!p) out.apart.push(a[0] + a[1] + '>' + b[0] + b[1]); else far = Math.max(far, H.length(p)); }
out.farthest = far;
out.faults = [];
const cross = (fixU, at, from, to) => { for (const w of H.WALLS) { if ((w.fix === 'u') === fixU) continue;
  if (w.at > from && w.at < to && at >= w.from && at <= w.to && !(w.gaps || []).some(g => at >= g[0] && at <= g[1])) out.faults.push('a walkway at ' + at + ' crosses the wall at ' + w.fix + '=' + w.at); } };
H.ALONG_V.forEach(l => cross(true, l[0], l[1], l[2])); H.ALONG_U.forEach(l => cross(false, l[0], l[1], l[2]));
for (const t of H.THINGS) { if (t.over || !t.box) continue; const b = t.box;
  H.ALONG_U.forEach(l => { if (l[0] > b[1] && l[0] < b[3] && l[1] < b[2] && l[2] > b[0]) out.faults.push('a walkway runs through a ' + t.kind); });
  H.ALONG_V.forEach(l => { if (l[0] > b[0] && l[0] < b[2] && l[1] < b[3] && l[2] > b[1]) out.faults.push('a walkway runs through a ' + t.kind); });
  for (const [r, i, s] of spots) if (s.walk && s.u > b[0] && s.u < b[2] && s.v > b[1] && s.v < b[3]) out.faults.push(r + i + ' stands inside a ' + t.kind); }
// room to walk: how near a walkway comes to a plant, and to the end of a rail where it goes through a gap
const gapOf = (a, b, lo, hi) => Math.max(0, lo - b, a - hi);
out.plants = []; out.doors = [];
for (const t of H.THINGS) { if (t.kind !== 'plant') continue; const b = t.box; let near = 1e9;
  H.ALONG_U.forEach(l => { near = Math.min(near, Math.max(gapOf(l[1], l[2], b[0], b[2]), gapOf(l[0], l[0], b[1], b[3]))); });
  H.ALONG_V.forEach(l => { near = Math.min(near, Math.max(gapOf(l[1], l[2], b[1], b[3]), gapOf(l[0], l[0], b[0], b[2]))); });
  if (near < H.CLEAR) out.plants.push(t.room + ' ' + b.slice(0, 2) + ': ' + near); }
const door = (fixU, at, from, to) => { for (const w of H.WALLS) { if ((w.fix === 'u') === fixU || !(w.at > from && w.at < to)) continue;
  for (const g of w.gaps || []) if (at >= g[0] && at <= g[1]) { const ends = [g[0] > w.from ? at - g[0] : 1e9, g[1] < w.to ? g[1] - at : 1e9];
    if (Math.min(...ends) < H.CLEAR) out.doors.push('the walkway at ' + at + ' passes the end of the wall ' + w.fix + '=' + w.at + ' at ' + Math.min(...ends)); } } };
H.ALONG_V.forEach(l => door(true, l[0], l[1], l[2])); H.ALONG_U.forEach(l => door(false, l[0], l[1], l[2]));
const back = H.WALLS.filter(w => w.fix === 'v' && w.at === 640 && w.h < H.TALL)[0];
out.hallDoor = 1210 - back.gaps[1][0];
// the hall: what the notice board says is not stood before
const face = H.boardFace(), over = b => b[0] < face[2] && b[2] > face[0] && b[1] < face[3] && b[3] > face[1];
out.hallOver = H.SPOTS.hall.filter(s => over(H.frogBox(s.u, s.v))).map(s => s.u + ',' + s.v);
out.farSide = over(H.frogBox(910, 830));
out.rooms = H.ROOMS.map(r => r.id);
out.inRoom = spots.filter(([r, i, s]) => H.roomAt(s.u, s.v) !== r).map(([r, i]) => r + i);

// the names over the frogs: a name 120 by 30 for a frog whose box has its middle at 500 and its top at 300
const tp = (placed, cx) => H.tagPlace(cx || 500, 300, 120, 30, placed, 1000);
out.tagFree = tp([]); out.tagFar = H.TAG_FAR;
out.tagAside = tp([[330, 266, 450, 296]]);                    // the name of a frog to the left reaches over this one's place
out.tagUp = tp([[300, 266, 700, 296]]);                       // a long name lies across it
out.tagNone = [tp([[300, 200, 700, 296]]), tp([[300, 266, 700, 296], [300, 232, 700, 262]])];      // a wall of names, and two rows of them
// five frogs in one doorway, 24 px apart: every name that is put is by its own frog and clear of the others
const names = []; out.tagDoor = { put: 0, far: 0, off: 0, clash: 0 };
for (let i = 0; i < 5; i++) { const cx = 400 + i * 24, at = tp(names, cx); if (!at) continue; out.tagDoor.put++;
  if (at[2] > 30 + H.TAG_FAR) out.tagDoor.far++;
  if (at[0] > cx + 8 || at[0] + 120 < cx - 8) out.tagDoor.off++;
  for (const r of names) if (at[0] < r[2] && at[0] + 120 > r[0] && at[1] < r[3] && at[1] + 30 > r[1]) out.tagDoor.clash++;
  names.push([at[0], at[1], at[0] + 120, at[1] + 30]); }

// who is where
const W = (o) => Object.assign({ kind: 'session', id: 'session:a', state: 'working' }, o);
out.roomOf = [H.roomOf(W({ act: 'lab' })), H.roomOf(W({ act: 'plan', wait: 'owner' })), H.roomOf(W({ doing: 'Grep' })), H.roomOf(W({ doing: 'editing the page' })),
              H.roomOf(W({ state: 'resting' })), H.roomOf(W({ act: 'bunk', state: 'resting' })), H.roomOf(W({ act: 'nowhere' })),
              H.roomOf({ kind: 'machine', id: 'pid:1', name: 'Blender, rendering films', state: 'working' }), H.roomOf({ kind: 'skill', id: 'tw-critic', name: 'tw-critic', state: 'working' })];
const lane = (branch, workers) => ({ branch, workers });
const floor = { roster: [{ id: 'pipeline', kind: 'skill', name: 'pipeline', busy: [], does: 'x' }, { id: 'tw-critic', kind: 'skill', name: 'tw-critic', busy: ['a', 'b'], does: 'x', home: 'lab' },
                         { id: 'agent:gamedesign', kind: 'agent', name: 'gamedesign', busy: ['a'], does: 'x', home: 'plan' }, { id: 'tw-master', kind: 'skill', name: 'tw-master', busy: [], does: 'x' }],
  lanes: [lane('a', [W({ id: 'session:1', act: 'work' }), { kind: 'skill', id: 'tw-critic', name: 'tw-critic', state: 'working', act: 'lab' },
                     { kind: 'agent', id: 'agent:Explore', uid: 'agent:Explore#1', name: 'Explore', state: 'working', act: 'lab' },
                     { kind: 'agent', id: 'agent:Explore', uid: 'agent:Explore#2', name: 'Explore', state: 'working', act: 'lab' },
                     { kind: 'agent', id: 'agent:gamedesign', uid: 'agent:gamedesign#9', name: 'gamedesign', state: 'working', act: 'plan' }]),
          lane('b', [{ kind: 'skill', id: 'tw-critic', name: 'tw-critic', state: 'working', act: 'lab' }, { kind: 'agent', id: 'agent:Explore', name: 'Explore', state: 'working' },
                     { kind: 'agent', id: 'agent:Explore', name: 'Explore', state: 'working' }])] };
const fr = H.frogs(floor);
out.keys = fr.map(f => f.key).sort();
out.critic = fr.filter(f => f.key === 'tw-critic').map(f => [f.branches.length, f.room]);
out.idle = fr.filter(f => f.w.state === 'idle').map(f => [f.key, f.room]);

// the frogs over time
const FROG = { anims: { walk_SW: { n: 12, fps: 8, stride: 5.6 }, walk_NW: { n: 12, fps: 8, stride: 5.1 }, sleep_in: { n: 21, fps: 8 }, sleep_loop: { n: 12, fps: 6 }, think: { n: 18, fps: 8 },
    office: { n: 24, fps: 8 }, magnifier: { n: 20, fps: 10 }, wrench: { n: 18, fps: 8 }, dance: { n: 24, fps: 12 } },
  walk: { SW: { anim: 'walk_SW', flip: false }, SE: { anim: 'walk_SW', flip: true }, NW: { anim: 'walk_NW', flip: false }, NE: { anim: 'walk_NW', flip: true } } };
const run = (sim, secs) => { for (let i = 0; i < secs * 10; i++) sim.step(0.1); };
const home = f => !!f.at && f.at.room === f.goal.room && f.at.i === f.goal.i;
const roster = ['pipeline', 'tw-critic', 'tw-master'].map(id => ({ id, kind: 'skill', name: id, busy: [], does: 'x', home: H.roomOf({ kind: 'skill', id, state: 'working' }) }));
const reading = (acts, extra) => ({ roster, lanes: [lane('a', acts.map((a, i) => W({ id: 'session:' + i, act: a, state: a === 'bunk' ? 'resting' : 'working' })).concat(extra || []))] });
let sim = new H.Sim(FROG);
sim.read(reading(['work', 'lab', 'bunk']));
const all = () => Object.values(sim.frogs);
out.boot = { n: all().length, walking: all().filter(f => f.st === 'walk').length, placed: all().every(home), asleep: all().filter(f => f.st === 'sleep').length };
out.rosterMats = roster.map((r, i) => sim.frogs[r.id].goal.i === i && sim.frogs[r.id].goal.room === 'bunk');
const desk = sim.frogs['session:0'].goal.i;
sim.read(reading(['lab', 'lab', 'bunk'])); run(sim, 4); const f0 = sim.frogs['session:0'];
out.walking = { st: f0.st, dir: f0.dir, pose: sim.pose(f0).anim };                        // it had been at its desk a while: it goes at once
run(sim, 90);
out.arrived = [f0.at && f0.at.room, sim.pose(f0).anim, home(f0)];
out.twoInLab = sim.frogs['session:0'].goal.i !== sim.frogs['session:1'].goal.i;
// one that only just arrived stays a moment before it follows the next change
const quick = new H.Sim(FROG); quick.read(reading(['work'])); quick.read(reading(['lab']));
const fq = quick.frogs['session:0']; for (let i = 0; i < 3000 && !home(fq); i++) quick.step(0.1);
// in seconds, not in DWELLs: asked to stay as long as the constant says, a constant of nought would pass
quick.read(reading(['shop'])); run(quick, 8); const stayed = fq.at ? fq.at.room : 'walking'; run(quick, 12);
out.dwell = [stayed, fq.st];
sim.read(reading(['work', 'lab', 'work'])); run(sim, 1);
out.woken = sim.frogs['session:2'].st;                                                    // called from its mat: up at once
run(sim, 120);
out.back = [sim.frogs['session:0'].goal.i === desk, home(sim.frogs['session:2']), sim.pose(sim.frogs['session:2']).anim];
sim.read(reading(['work', 'bunk', 'work'])); run(sim, 120);
out.toBed = [sim.frogs['session:1'].st, sim.pose(sim.frogs['session:1']).anim, sim.pose(sim.frogs['session:1']).flip];
// one leaves, one comes
sim.read(reading(['work', 'bunk'], [W({ id: 'session:new', act: 'shop' })])); run(sim, 0.5);
const nf = sim.frogs['session:new'], gone = sim.frogs['session:2'];
out.gate = { cameAt: [Math.round(nf.u), Math.round(nf.v)], gate: [H.GATE.u, H.GATE.v], leaving: !!(gone && gone.leaving) };
run(sim, 150);
out.gateAfter = { left: !sim.frogs['session:2'], arrived: home(sim.frogs['session:new']) && sim.frogs['session:new'].at.room === 'shop' };
// the feet: the loop turns with the ground covered
sim = new H.Sim(FROG); sim.read(reading(['bunk'])); sim.read(reading(['work']));
let f = sim.frogs['session:0'], dist = 0, last = [f.u, f.v], p0 = f.phase, strides = 0;
for (let i = 0; i < 4000 && f.st !== 'at'; i++) { sim.step(0.05); const d = Math.abs(f.u - last[0]) + Math.abs(f.v - last[1]); dist += d; if (f.st === 'walk') strides += d / sim.loop(f.dir).stride; last = [f.u, f.v]; }
out.feet = { dist: Math.round(dist), phase: +(f.phase - p0).toFixed(2), strides: +strides.toFixed(2) };
// a room with more frogs than spots
sim = new H.Sim(FROG); const many = []; for (let i = 0; i < 9; i++) many.push('hall');
sim.read({ roster: [], lanes: [lane('a', many.map((a, i) => W({ id: 'session:' + i, act: 'work', wait: 'owner' })))] });
out.crowd = { n: Object.keys(sim.frogs).length, spots: H.SPOTS.hall.length, placed: Object.values(sim.frogs).every(home) };
// the made-up floor, eighteen readings long
sim = new H.Sim(FROG); sim.read(H.demo(0)); let maxWalkers = 0; const seen = new Set();
for (let beat = 1; beat <= 18; beat++) { sim.read(H.demo(beat * 20)); for (let i = 0; i < 200; i++) { sim.step(0.1); } maxWalkers = Math.max(maxWalkers, Object.values(sim.frogs).filter(f => f.st === 'walk').length);
  Object.values(sim.frogs).forEach(f => { if (f.at) seen.add(f.at.room); }); }
run(sim, 150);
const seats = {}; let shared = 0; Object.values(sim.frogs).forEach(f => { const k = f.goal.room + f.goal.i; if (seats[k]) shared++; seats[k] = 1; });
out.demo = { stuck: Object.values(sim.frogs).filter(f => !home(f)).map(f => f.key), shared, maxWalkers, rooms: [...seen].sort(), noClip: Object.values(sim.frogs).filter(f => !sim.pose(f).anim).length };
console.log(JSON.stringify(out));
'''


def main():
    node = shutil.which('node')
    if not node:
        print("      (no node on this machine: the house's cases were not run)")
        return 0
    with tempfile.TemporaryDirectory() as tmp:
        js = Path(tmp) / 'harness.js'
        js.write_text(HARNESS, encoding='utf-8')
        p = subprocess.run([node, str(js), str(HERE / 'static' / 'house.js')], capture_output=True)
    try:
        o = json.loads(p.stdout.decode() or 'null')
    except ValueError:
        o = None
    if not o:
        case('house: its rules run under node', False, p.stderr.decode()[-600:])
        return 1

    case('plan: seven rooms, and every spot is in the room it belongs to', len(o['rooms']) == 7 and not o['inRoom'], (o['rooms'], o['inRoom']))
    case('plan: every spot can be walked to from the gate and from every other spot', not o['unreachable'] and not o['apart'], (o['unreachable'], o['apart'][:5]))
    case('plan: every step of every way runs along one axis, so only the four diagonal walk loops are ever needed', not o['diagonal'], o['diagonal'])
    case('plan: no walkway crosses a wall except at a gap or runs through furniture, and no spot stands inside any', not o['faults'], o['faults'][:6])
    case('plan: a frog steps onto its mat going down-right, the way the mirrored falling-asleep clip starts', not o['matEntry'], o['matEntry'])
    case('plan: no walkway comes nearer a plant than a frog is wide, nor nearer the end of a rail it passes; by the hall\'s right corner the rail ends twice that far from the walkway',
         not o['plants'] and not o['doors'] and o['hallDoor'] >= 100, (o['plants'], o['doors'], o['hallDoor']))
    case('plan: nobody who waits in the hall is drawn over what the notice board says (its words and its number); on the far side of the rug, where they stood, one was',
         not o['hallOver'] and o['farSide'], (o['hallOver'], o['farSide']))
    case('plan: the longest walk in the house is under 3,500 sprite pixels', 1500 < o['farthest'] < 3500, o['farthest'])

    aside, up = o['tagAside'], o['tagUp']
    case('names: a frog\'s name stands over it; when that place is taken it steps aside at the same height and still reaches its frog, or goes up onto the name in the way, never further than its own height and a little',
         o['tagFree'] == [440, 266, 0] and aside and aside[2] == 0 and aside[0] != 440 and aside[0] <= 508 and aside[0] + 120 >= 492
         and up and 0 < up[2] <= 30 + o['tagFar'] and up[1] == 233, (o['tagFree'], aside, up))
    case('names: with no place that near its frog a name is left out, not pushed up past the others; of five frogs in one doorway some are named, each by its own frog, and no two names lie over each other',
         o['tagNone'] == [None, None] and 2 <= o['tagDoor']['put'] < 5 and not o['tagDoor']['far'] and not o['tagDoor']['off'] and not o['tagDoor']['clash'], (o['tagNone'], o['tagDoor']))
    case('who: a worker is in the room its work names; whoever waits on the owner is in the hall; whoever rests is in the bunkhouse',
         o['roomOf'][:2] == ['lab', 'hall'] and o['roomOf'][4:6] == ['bunk', 'bunk'], o['roomOf'])
    case('who: a reading from before the rooms is read by the last tool call, and a room nobody knows is the workroom',
         o['roomOf'][2:4] == ['lab', 'work'] and o['roomOf'][6:] == ['work', 'studio', 'lab'], o['roomOf'])
    case('who: a skill called from two branches is one frog, two agents of one type are two, and an agent on the roster is its roster frog',
         o['keys'] == sorted(['session:1', 'tw-critic', 'agent:Explore#1', 'agent:Explore#2', 'agent:gamedesign', 'agent:Explore#1@b', 'agent:Explore#2@b', 'pipeline', 'tw-master'])
         and o['critic'] == [[2, 'lab']], (o['keys'], o['critic']))
    case('who: whoever on the roster nothing calls is asleep in the bunkhouse', sorted(o['idle']) == [['pipeline', 'bunk'], ['tw-master', 'bunk']], o['idle'])

    b = o['boot']
    case('time: on the first reading everyone is already in place and nobody walks', b['n'] == 6 and b['walking'] == 0 and b['placed'] and b['asleep'] == 4, b)
    case('time: everyone on the roster sleeps on the mat of its place on the roster', all(o['rosterMats']), o['rosterMats'])
    case('time: a frog whose work changes walks, drawn with a walk loop for the way it is going', o['walking']['st'] == 'walk' and o['walking']['pose'] in ('walk_SW', 'walk_NW'), o['walking'])
    case('time: and arrives, at a spot no other frog has, doing what the room is for', o['arrived'] == ['lab', 'magnifier', True] and o['twoInLab'], (o['arrived'], o['twoInLab']))
    case('time: a frog that only just arrived is still there eight seconds on, and has left for its new work twenty seconds on', o['dwell'] == ['lab', 'walk'], o['dwell'])
    case('time: a frog called from its mat gets up at once', o['woken'] == 'up', o['woken'])
    case('time: a frog that comes back to the workroom has its own desk again', o['back'] == [True, True, 'office'], o['back'])
    case('time: a frog sent to bed lies down on its mat, mirrored, and sleeps', o['toBed'] == ['sleep', 'sleep_loop', True], o['toBed'])
    case('time: a new worker comes in at the gate, and one that is gone walks out by it and is no more',
         o['gate']['cameAt'][1] == o['gate']['gate'][1] and abs(o['gate']['cameAt'][0] - o['gate']['gate'][0]) < 80 and o['gate']['leaving']
         and o['gateAfter'] == {'left': True, 'arrived': True}, (o['gate'], o['gateAfter']))
    case('time: the walk loop turns with the ground covered, so the feet do not slide', abs(o['feet']['phase'] - o['feet']['strides']) < 0.01 * o['feet']['strides'] and o['feet']['dist'] > 500, o['feet'])
    case('time: a room with more frogs than spots still seats every one', o['crowd']['n'] == 9 and o['crowd']['n'] > o['crowd']['spots'] and o['crowd']['placed'], o['crowd'])
    d = o['demo']
    case('demo: over eighteen readings every room is used, frogs walk, and in the end nobody is stuck, shares a spot or lacks a clip',
         not d['stuck'] and d['shared'] == 0 and d['maxWalkers'] >= 2 and len(d['rooms']) == 7 and d['noClip'] == 0, d)

    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
