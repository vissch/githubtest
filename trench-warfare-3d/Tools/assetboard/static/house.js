// The house: the crew's seven rooms, and the frogs that walk between them. A worker is in the room of the work it is
// at (the reading says which: `act`, src_acts.py), at a spot of its own there, and walks when that changes.
// The house is an isometric cutaway because the frogs are drawn that way: the desk's edges run at 30 degrees, and the
// walk loops that exist are the four diagonals. So every walkway runs along one of the plan's two axes, and no frog
// ever walks straight up the screen (there is no North loop).
// Tables and pure functions, no DOM, so test_house.py runs them under node; housedraw.js draws what they say.
(function (root) {
  'use strict';
  // the plan is in sprite pixels: u runs down-right on the screen, v down-left, z up
  var COS = Math.cos(Math.PI / 6), SIN = 0.5;
  function iso(u, v, z) { return [(u - v) * COS, (u + v) * SIN - (z || 0)]; }
  function plan(x, y) { return [x / (2 * COS) + y, y - x / (2 * COS)]; }
  function hash(s) { var h = 0; for (var i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) >>> 0; return h; }
  function clamp(x, a, b) { return x < a ? a : x > b ? b : x; }

  // three by three cells with two opposite corners left out: the hall in the middle, six rooms round it
  var U = [0, 560, 1260, 2020], V = [0, 640, 1160, 1800];
  var ROOMS = [
    { id: 'lab', a: 0, b: 1, name: 'Lab', does: 'reading, searching, reviews, tests', anim: 'magnifier' },
    { id: 'shop', a: 1, b: 0, name: 'Workshop', does: 'builds, Unity, tools, machines', anim: 'wrench' },
    { id: 'studio', a: 0, b: 2, name: 'Studio', does: 'animation, films, VFX, art', anim: 'dance' },
    { id: 'hall', a: 1, b: 1, name: 'Hall', does: 'the way in, and waiting on the owner', anim: 'think' },
    { id: 'bunk', a: 2, b: 0, name: 'Bunkhouse', does: 'resting and idle', anim: 'sleep_loop' },
    { id: 'work', a: 1, b: 2, name: 'Workroom', does: 'code, docs, commits', anim: 'office' },
    { id: 'plan', a: 2, b: 1, name: 'War room', does: 'plans, briefing agents', anim: 'think' }
  ];
  var ROOM = {};
  ROOMS.forEach(function (r) { r.u0 = U[r.a]; r.u1 = U[r.a + 1]; r.v0 = V[r.b]; r.v1 = V[r.b + 1]; ROOM[r.id] = r; });
  function roomAt(u, v) { for (var i = 0; i < ROOMS.length; i++) { var r = ROOMS[i]; if (u >= r.u0 && u < r.u1 && v >= r.v0 && v < r.v1) return r.id; } return null; }

  // walls: [axis the wall runs along is the OTHER one: 'u' means u is fixed, at, from, to, gaps]. The tall ones are the
  // outer back walls (nothing stands behind them); a wall between two rooms is low, or it would hide the room behind.
  var TALL = 170, LOW = 26, CURB = 10;
  var WALLS = [
    { fix: 'u', at: 0, from: 640, to: 1800, h: TALL }, { fix: 'v', at: 640, from: 0, to: 560, h: TALL },
    { fix: 'u', at: 560, from: 0, to: 640, h: TALL }, { fix: 'v', at: 0, from: 560, to: 2020, h: TALL },
    { fix: 'u', at: 560, from: 640, to: 1800, h: LOW, gaps: [[640, 740], [1060, 1260]] },
    { fix: 'v', at: 1160, from: 0, to: 2020, h: LOW, gaps: [[460, 660], [1160, 1360]] },
    { fix: 'v', at: 640, from: 560, to: 2020, h: LOW, gaps: [[560, 660], [1100, 1360]] },      // the hall's back rail stops well short of the walkway at u 1210: a frog is 106 wide on the plan
    { fix: 'u', at: 1260, from: 0, to: 1800, h: LOW, gaps: [[540, 740], [1060, 1260]] },
    { fix: 'v', at: 1800, from: 0, to: 1260, h: CURB }, { fix: 'u', at: 2020, from: 0, to: 1160, h: CURB }
  ];

  // walkways: where a frog may walk. Each runs along one axis: [the fixed coordinate, from, to]
  var ALONG_U = [[30, 1310, 1760], [215, 700, 1210], [510, 780, 1210], [590, 610, 1310], [627, 1310, 1760], [690, 225, 1930], [900, 610, 1210], [1110, 225, 1930],
                 [1210, 60, 1990], [1410, 610, 1245], [1480, 60, 510], [1600, 610, 1245], [1750, 60, 510], [1790, 610, 1245]];     // v fixed
  var ALONG_V = [[60, 1210, 1750], [225, 690, 1110], [510, 690, 1750], [610, 590, 1790], [780, 215, 590], [910, 900, 1110], [1210, 215, 1210],
                 [1245, 1210, 1790], [1310, 30, 1210], [1420, 690, 1110], [1535, 30, 627], [1760, 30, 627], [1930, 690, 1110]];                         // u fixed
  var GATE = { u: 1990, v: 1210 };      // where the path from the hall's front corner leaves the picture
  var CLEAR = 50;                       // how far a walkway stays from a plant and from the end of a rail it passes, on the plan

  // spots: where a frog does its work. (u, v) is where it is drawn; (au, av) the point on a walkway it comes from.
  // walk: it steps from there onto the spot (a desk is not stepped onto: the frog is simply drawn seated).
  function spot(u, v, au, av, more) { var s = { u: u, v: v, au: au, av: av, flip: false, walk: true }; for (var k in more || {}) s[k] = more[k]; return s; }
  var SPOTS = { lab: [], shop: [], studio: [], hall: [], bunk: [], work: [], plan: [] };
  // lab: two benches run down-left; a frog stands at a bench's near side and holds its glass over it
  [760, 910, 1060].forEach(function (v) { SPOTS.lab.push(spot(395, v, 510, v)); });
  [760, 910, 1060].forEach(function (v) { SPOTS.lab.push(spot(165, v, 225, v)); });
  // workshop: the bench in the middle, then the machines along the two back walls
  SPOTS.shop.push(spot(950, 455, 950, 510), spot(1085, 455, 1085, 510, { flip: true }), spot(720, 310, 780, 310), spot(720, 470, 780, 470, { flip: true }),
                  spot(705, 160, 705, 215), spot(865, 160, 865, 215, { flip: true }), spot(1025, 160, 1025, 215), spot(1180, 160, 1180, 215, { flip: true }));
  // studio: six marks on the stage, either side of the line across it
  [[290, 1400], [290, 1560], [170, 1400], [410, 1560], [410, 1400], [170, 1560]].forEach(function (m, i) { SPOTS.studio.push(spot(m[0], m[1], m[0], 1480, { flip: i % 2 === 1 })); });
  // hall: on the rug before the notice board, for whoever waits on the owner. They stand on the near side of the walkway
  // across the rug: on the far side (v 830) a frog's head was drawn over the board's number, the one thing the board says
  SPOTS.hall.push(spot(910, 970, 910, 900), spot(820, 970, 820, 900), spot(1000, 970, 1000, 900, { flip: true }), spot(730, 970, 730, 900), spot(1090, 970, 1090, 900, { flip: true }));
  // bunkhouse: three columns of mats. A frog steps onto its mat going down-right, which is how the falling-asleep
  // clip starts once mirrored, and lies with its head that way
  [[1420, 1310], [1645, 1535], [1870, 1760]].forEach(function (c) {
    [80, 164, 248, 332, 416, 500, 584].forEach(function (v) { SPOTS.bunk.push(spot(c[0], v, c[1], v, { flip: true, bed: true })); });
  });
  // workroom: three rows of four desks; the frog sits on the near side, so it comes along the row in front
  [1335, 1525, 1715].forEach(function (v) { [725, 885, 1045, 1205].forEach(function (u) { SPOTS.work.push(spot(u, v, u - 38, v + 75, { walk: false, desk: true })); }); });
  // war room: round the table, the two sides that face it first
  SPOTS.plan.push(spot(1620, 775, 1620, 690), spot(1740, 775, 1740, 690), spot(1475, 860, 1420, 860, { flip: true }), spot(1475, 940, 1420, 940, { flip: true }),
                  spot(1570, 1030, 1570, 1110), spot(1680, 1030, 1680, 1110, { flip: true }), spot(1790, 1030, 1790, 1110), spot(1885, 860, 1930, 860), spot(1885, 940, 1930, 940));

  // what stands in the rooms, on the plan: [u0, v0, u1, v1, height]; `over` may be walked on. housedraw.js draws each by its kind.
  // A plant stands CLEAR (50) or more from every walkway: the two in the hall stood in the corners where two walkways
  // meet, 32 to 42 from them, and whoever walked by was drawn through the leaves.
  var THINGS = [
    { kind: 'bench', room: 'lab', box: [60, 720, 110, 1085, 46] }, { kind: 'bench', room: 'lab', box: [290, 720, 340, 1085, 46] },
    { kind: 'machine', room: 'shop', box: [640, 14, 770, 100, 124], look: 0 }, { kind: 'machine', room: 'shop', box: [800, 14, 930, 100, 104], look: 1 },
    { kind: 'machine', room: 'shop', box: [960, 14, 1090, 100, 132], look: 2 }, { kind: 'machine', room: 'shop', box: [1120, 14, 1245, 100, 96], look: 3 },
    { kind: 'machine', room: 'shop', box: [574, 250, 660, 370, 112], look: 4 }, { kind: 'machine', room: 'shop', box: [574, 410, 660, 530, 92], look: 5 },
    { kind: 'workbench', room: 'shop', box: [900, 330, 1130, 400, 44] },
    { kind: 'stage', room: 'studio', disc: [290, 1480, 178, 12], over: true },
    { kind: 'lamp', room: 'studio', box: [95, 1285, 125, 1315, 150] }, { kind: 'lamp', room: 'studio', box: [95, 1650, 125, 1680, 150] },
    { kind: 'camera', room: 'studio', box: [440, 1640, 476, 1676, 96] },
    { kind: 'board', room: 'hall', box: [790, 742, 1030, 758, 150] }, { kind: 'rug', room: 'hall', box: [700, 790, 1120, 1050, 0], over: true },
    { kind: 'plant', room: 'hall', box: [1052, 744, 1092, 784, 70] }, { kind: 'plant', room: 'hall', box: [668, 1006, 708, 1046, 70] },
    { kind: 'table', room: 'plan', box: [1530, 830, 1830, 970, 48] }, { kind: 'easel', room: 'plan', box: [1960, 700, 1976, 800, 140] },
    { kind: 'plant', room: 'plan', box: [1840, 740, 1880, 780, 70] }
  ];
  // a mat lies where the sleeping frog is drawn: the clip leaves it up and back from the point it stood on (84 behind to 37 ahead, 16 to the far side)
  SPOTS.bunk.forEach(function (s, i) { THINGS.push({ kind: 'mat', room: 'bunk', box: [s.u - 98, s.v - 52, s.u + 62, s.v + 20, 9], over: true, spot: i }); });
  SPOTS.work.forEach(function (s, i) { THINGS.push({ kind: 'desk', room: 'work', box: [s.u - 95, s.v - 88, s.u + 17, s.v + 56, 0], spot: i }); });

  // the walkways as a graph: a node wherever two cross, where one ends, and where a spot is reached from
  var NODES = [], INDEX = {};
  function node(u, v) { var k = u + ',' + v; if (INDEX[k] == null) { INDEX[k] = NODES.length; NODES.push({ u: u, v: v, to: [] }); } return INDEX[k]; }
  function link(a, b) { if (a === b) return; var A = NODES[a], B = NODES[b], d = Math.abs(A.u - B.u) + Math.abs(A.v - B.v); A.to.push([b, d]); B.to.push([a, d]); }
  (function () {
    var onU = ALONG_U.map(function (l) { return [l[1], l[2]]; }), onV = ALONG_V.map(function (l) { return [l[1], l[2]]; });
    ALONG_U.forEach(function (lu, i) { ALONG_V.forEach(function (lv, j) {
      if (lv[0] >= lu[1] && lv[0] <= lu[2] && lu[0] >= lv[1] && lu[0] <= lv[2]) { onU[i].push(lv[0]); onV[j].push(lu[0]); }
    }); });
    Object.keys(SPOTS).forEach(function (room) { SPOTS[room].forEach(function (s, n) {
      var i = -1, j = -1;
      ALONG_U.forEach(function (l, k) { if (l[0] === s.av && s.au >= l[1] && s.au <= l[2]) i = k; });
      ALONG_V.forEach(function (l, k) { if (l[0] === s.au && s.av >= l[1] && s.av <= l[2]) j = k; });
      if (i < 0 && j < 0) throw new Error('house: spot ' + room + ' ' + n + ' is reached from no walkway');
      if (i >= 0) onU[i].push(s.au); else onV[j].push(s.av);
    }); });
    function uniq(a) { return a.sort(function (x, y) { return x - y; }).filter(function (x, i) { return !i || x !== a[i - 1]; }); }
    ALONG_U.forEach(function (l, i) { var at = uniq(onU[i]); for (var k = 1; k < at.length; k++) link(node(at[k - 1], l[0]), node(at[k], l[0])); });
    ALONG_V.forEach(function (l, j) { var at = uniq(onV[j]); for (var k = 1; k < at.length; k++) link(node(l[0], at[k - 1]), node(l[0], at[k])); });
    Object.keys(SPOTS).forEach(function (room) { SPOTS[room].forEach(function (s) {
      s.room = room; s.an = node(s.au, s.av);
      if (s.walk) { s.n = NODES.length; NODES.push({ u: s.u, v: s.v, to: [], leaf: true }); link(s.n, s.an); } else s.n = s.an;
    }); });
  })();
  var GATE_NODE = node(GATE.u, GATE.v);

  // the shortest way from one node to another, as the nodes passed. A spot is a dead end, so no way leads over one.
  function route(from, to) {
    var n = NODES.length, dist = [], prev = [], done = [], i;
    for (i = 0; i < n; i++) dist[i] = Infinity;
    dist[from] = 0;
    for (;;) {
      var best = -1;
      for (i = 0; i < n; i++) if (!done[i] && dist[i] < Infinity && (best < 0 || dist[i] < dist[best])) best = i;
      if (best < 0 || best === to) break;
      done[best] = true;
      if (NODES[best].leaf && best !== from) continue;
      for (i = 0; i < NODES[best].to.length; i++) { var e = NODES[best].to[i], d = dist[best] + e[1]; if (d < dist[e[0]]) { dist[e[0]] = d; prev[e[0]] = best; } }
    }
    if (dist[to] === Infinity) return null;
    var path = [to];
    while (path[0] !== from) path.unshift(prev[path[0]]);
    return path;
  }
  function length(path) { var d = 0; for (var i = 1; i < path.length; i++) d += Math.abs(NODES[path[i]].u - NODES[path[i - 1]].u) + Math.abs(NODES[path[i]].v - NODES[path[i - 1]].v); return d; }
  // which of the four walk loops a step along the plan is drawn with
  function heading(a, b) { return b.u > a.u ? 'SE' : b.u < a.u ? 'NW' : b.v > a.v ? 'SW' : 'NE'; }

  // which room a worker is in. A reading from before `act` (another station's older build) is read by its last tool call.
  var HOME = { 'tw-critic': 'lab', 'tw-bug-catcher': 'lab', 'tw-balance-sim': 'lab', 'tw-master': 'plan', pipeline: 'plan', 'tw-vfx-sheets': 'studio',
    'tw-destruction-vfx': 'studio', 'tw-character-sim': 'studio', 'tw-env-sim': 'studio', 'tw-vehicle-sim': 'shop', 'tw-optimizer': 'shop',
    'unity-pipeline': 'shop', 'agent:gamedesign': 'plan', 'agent:relay': 'plan', 'agent:Explore': 'lab', 'agent:Plan': 'plan' };
  var TOOL = { Read: 'lab', Grep: 'lab', Glob: 'lab', WebFetch: 'lab', WebSearch: 'lab', Edit: 'work', MultiEdit: 'work', Write: 'work', NotebookEdit: 'work',
    Bash: 'shop', PowerShell: 'shop', Agent: 'plan', Task: 'plan', TodoWrite: 'plan', SendMessage: 'plan', AskUserQuestion: 'hall', ExitPlanMode: 'hall' };
  function roomOf(w) {
    if (w.wait === 'owner') return 'hall';
    if (w.act && SPOTS[w.act]) return w.act;
    if (w.state !== 'working') return 'bunk';
    if (w.kind === 'session') return TOOL[w.doing] || 'work';
    if (w.kind === 'machine') return /blender|film/i.test(w.name || '') ? 'studio' : /test|gate/i.test(w.name || '') ? 'lab' : 'shop';
    return HOME[w.id] || HOME[w.name] || 'work';
  }
  // how a worker is drawn apart from the others. Who made it: Claude's are the frog as drawn, a Codex frog is blue and a
  // Grok frog amber (the sheet's colours turned by `hue` degrees), each with its letter over it; a leg of the relay
  // has an R. An agent another worker sent is smaller than the one who sent it. `away`: it works on the other station.
  var VENDOR = { codex: { hue: 100, badge: 'X', name: 'Codex' }, grok: { hue: -85, badge: 'G', name: 'Grok' } };
  function look(w) {
    var v = VENDOR[w.vendor] || null, leg = w.id === 'agent:relay-leg';
    return { hue: v ? v.hue : 0, vendor: v ? w.vendor : '', badge: v ? v.badge : leg ? 'R' : '', scale: w.kind === 'agent' && w.parent && !leg ? 0.8 : 1, away: !!w.host };
  }
  // who made a worker and where it runs, as the few words the card and the panel show: 'Codex', its model, the other
  // station's name, since when
  function made(w) {
    var v = VENDOR[w.vendor], out = [];
    if (v) out.push(v.name);
    if (w.model) out.push(String(w.model).replace(/^claude-/, ''));
    if (w.host) out.push('on ' + w.host);
    if (w.since) out.push('since ' + w.since);
    return out;
  }
  // the floor in one line, over the house: how many are at work and what they are, and whether the other station
  // still speaks. { text, stale }; text is '' for a reading from before the floor was counted.
  function span(s) { return s < 90 ? s + ' s' : s < 5400 ? Math.round(s / 60) + ' min' : Math.round(s / 3600) + ' h'; }
  function tally(ops) {
    var f = ops && ops.floor; if (!f) return { text: '', stale: false };
    var parts = [], stale = false;
    function add(n, one, many) { if (n) parts.push(n + ' ' + (n === 1 ? one : many || one + 's')); }
    add(f.sessions, 'session'); add(f.subagents, 'subagent'); add(f.relay, 'relay leg'); add(f.codex, 'Codex', 'Codex'); add(f.grok, 'Grok', 'Grok');
    add(f.skills, 'skill'); add(f.machines, 'machine');
    var text = f.working ? f.working + ' at work: ' + parts.join(', ') : 'nobody at work';
    (ops.stations || []).forEach(function (s) {
      if (s.stale) stale = true;
      text += ' · ' + s.host + (s.stale ? ' silent for ' + span(s.age) + ' (its frogs rest)' : ' seen ' + span(s.age) + ' ago');
    });
    return { text: text, stale: stale };
  }
  // everyone in a reading, a frog each: the workers on the branches (a skill is one frog wherever it is called from,
  // an agent one per run) and whoever on the roster nothing calls. An agent on the roster is its roster frog while one
  // run of it is going, so it gets up from its own mat; a second run of it at the same time is a frog of its own.
  function frogs(ops) {
    var out = [], seen = {}, listed = {};
    (ops.roster || []).forEach(function (r) { listed[r.id] = true; });
    (ops.lanes || []).forEach(function (l) {
      var nth = {};
      (l.workers || []).forEach(function (w) {
        var key = w.id;
        if (w.kind === 'agent' && !(listed[w.id] && !seen[w.id])) key = w.uid || w.id + '#' + (nth[w.id] = (nth[w.id] || 0) + 1) + '@' + l.branch;
        if (seen[key]) { if (seen[key].branches.indexOf(l.branch) < 0) seen[key].branches.push(l.branch); return; }
        out.push(seen[key] = { key: key, w: w, where: l.branch, branches: [l.branch], room: roomOf(w) });
        seen[w.id] = seen[w.id] || seen[key];
      });
    });
    (ops.roster || []).forEach(function (r) {
      if (seen[r.id]) return;
      var busy = r.busy && r.busy.length, w = { id: r.id, kind: r.kind, name: r.name, what: r.does, state: busy ? 'working' : 'idle' };
      if (busy) w.act = r.home || HOME[r.id] || 'work';
      out.push(seen[r.id] = { key: r.id, w: w, where: busy ? r.busy.join(', ') : '', branches: busy ? r.busy : [], room: roomOf(w) });
    });
    return out;
  }

  // the frogs over time. read(ops) says where each belongs; step(dt) walks them there.
  var DWELL = 12;             // seconds a frog stays at a spot before it follows a change of work (a quick look-up is not a move)
  var TRIP = 16;              // a walk is paced to take about this long, within PACE
  var PACE = [1.8, 2.6];      // how much faster than the loop as drawn (8 frames a second) a frog may walk
  var WALK_FPS = 8, UP = 1.6; // a frog gets up by the falling-asleep clip played backwards, this much faster
  function Sim(frog) {
    var self = this;
    this.t = 0; this.frogs = {}; this.booted = false; this.claim = {}; this.kept = {}; this.roster = {}; this.rosterN = 0;
    ROOMS.forEach(function (r) { self.claim[r.id] = {}; self.kept[r.id] = {}; });
    this.anims = (frog && frog.anims) || {}; this.walk = (frog && frog.walk) || {};
  }
  Sim.prototype.loop = function (dir) { var w = this.walk[dir], a = w && this.anims[w.anim]; return { stride: (a && a.stride) || 5.5, n: (a && a.n) || 12 }; };
  Sim.prototype.secs = function (anim) { var a = this.anims[anim]; return a ? a.n / a.fps : 2.6; };
  function same(a, b) { return a === b || (!!a && !!b && a.room === b.room && a.i === b.i); }
  // a frog's own spot in a room: the one it had, else the one its name falls on, else the next free one. Everyone on
  // the roster has the mat of its place on the roster, so the bunkhouse reads the same on every station.
  Sim.prototype.seat = function (f, room) {
    var n = SPOTS[room].length, claim = this.claim[room], kept = this.kept[room], have = f.seats[room], i, k, soft;
    function take(i) { claim[i] = (claim[i] || 0) + 1; kept[i] = f.key; f.seats[room] = i; return i; }
    if (have != null && !claim[have]) return take(have);
    var ri = this.roster[f.key], want = hash(f.key) % n;
    if (room === 'bunk') want = ri != null ? ri % n : (this.rosterN + hash(f.key) % Math.max(1, n - this.rosterN)) % n;
    for (soft = 1; soft >= 0; soft--)
      for (k = 0; k < n; k++) { i = (want + k) % n; if (!claim[i] && (!soft || !kept[i] || kept[i] === f.key || !this.frogs[kept[i]])) return take(i); }
    // a full room: some share a spot (housedraw.js sets them apart). The spot with the fewest on it, from its own on:
    // nineteen in the lab's six stood five deep on one bench and nobody could tell who was who
    var best = want;
    for (k = 0; k < n; k++) { i = (want + k) % n; if ((claim[i] || 0) < (claim[best] || 0)) best = i; }
    return take(best);
  };
  Sim.prototype.release = function (f) { if (f.goal) { var c = this.claim[f.goal.room]; if (c[f.goal.i]) c[f.goal.i]--; } };
  Sim.prototype.read = function (ops) {
    var self = this, list = frogs(ops), live = {};
    this.roster = {}; (ops.roster || []).forEach(function (r, i) { self.roster[r.id] = i; }); this.rosterN = (ops.roster || []).length;
    list.sort(function (a, b) { return a.key < b.key ? -1 : a.key > b.key ? 1 : 0; });       // a fixed order: the same reading seats everyone the same way
    list.forEach(function (it) {
      live[it.key] = true;
      var f = self.frogs[it.key], fresh = !f;
      if (fresh) f = self.frogs[it.key] = { key: it.key, seats: {}, st: 'at', at: null, goal: null, node: GATE_NODE, u: GATE.u, v: GATE.v, dir: 'NW', phase: 0, t0: self.t - DWELL, pace: PACE[0], n: hash(it.key) };
      f.w = it.w; f.where = it.where; f.branches = it.branches; f.leaving = false;
      if (!f.goal || f.goal.room !== it.room) { self.release(f); f.goal = { room: it.room, i: self.seat(f, it.room) }; }
      if (fresh && !self.booted) self.put(f);      // the first reading: everyone is already where they belong
    });
    Object.keys(this.frogs).forEach(function (k) { var f = self.frogs[k]; if (!live[k] && !f.leaving) { f.leaving = true; self.release(f); f.goal = null; } });
    this.booted = true;
  };
  Sim.prototype.put = function (f) {
    var s = SPOTS[f.goal.room][f.goal.i];
    f.at = f.goal; f.node = s.n; f.u = s.u; f.v = s.v; f.path = null; f.st = s.bed ? 'sleep' : 'at'; f.t0 = this.t - DWELL - (f.n % 997) / 100;
  };
  Sim.prototype.depart = function (f, want) {
    var to = want === 'gate' ? GATE_NODE : SPOTS[want.room][want.i].n, path = route(f.node, to);
    f.dest = want; f.at = null;
    if (!path || path.length < 2) return this.arrive(f);
    f.path = path; f.leg = 0; f.prog = 0; f.st = 'walk';
    f.pace = clamp(length(path) / (5.5 * WALK_FPS * TRIP), PACE[0], PACE[1]);
  };
  Sim.prototype.arrive = function (f) {
    if (f.dest === 'gate') { f.st = 'gone'; delete this.frogs[f.key]; return; }
    var s = SPOTS[f.dest.room][f.dest.i];
    f.at = f.dest; f.node = s.n; f.u = s.u; f.v = s.v; f.path = null; f.t0 = this.t; f.st = s.bed ? 'down' : 'at';
  };
  Sim.prototype.advance = function (f, dt) {
    while (dt > 0 && f.st === 'walk') {
      var a = NODES[f.path[f.leg]], b = NODES[f.path[f.leg + 1]], L = Math.abs(b.u - a.u) + Math.abs(b.v - a.v);
      f.dir = heading(a, b);
      var loop = this.loop(f.dir), speed = loop.stride * WALK_FPS * f.pace, need = (L - f.prog) / speed, go = Math.min(dt, need);
      f.prog += speed * go; f.phase += speed * go / loop.stride; dt -= go;     // the loop turns with the ground covered, so the feet do not slide
      var k = L ? Math.min(1, f.prog / L) : 1;
      f.u = a.u + (b.u - a.u) * k; f.v = a.v + (b.v - a.v) * k;
      if (go < need) break;
      f.leg++; f.prog = 0; f.node = f.path[f.leg];
      var want = f.leaving ? 'gate' : f.goal;
      if (f.leg === f.path.length - 1) { if (same(f.dest, want)) this.arrive(f); else this.depart(f, want); }
      else if (!same(f.dest, want) && !NODES[f.node].leaf) this.depart(f, want);          // its work changed on the way: turn at this corner
    }
  };
  Sim.prototype.step = function (dt) {
    var self = this;
    this.t += dt;
    Object.keys(this.frogs).forEach(function (key) {
      var f = self.frogs[key], want = f.leaving ? 'gate' : f.goal;
      if (f.st === 'walk') return self.advance(f, dt);
      if (f.st === 'up') { if (self.t - f.t0 >= self.secs('sleep_in') / UP) self.depart(f, want); return; }
      if (f.st === 'down' && self.t - f.t0 >= self.secs('sleep_in')) { f.st = 'sleep'; f.t0 = self.t; }
      if (f.at && same(f.at, want)) return;
      if (f.st === 'sleep' || f.st === 'down') { f.st = 'up'; f.t0 = self.t; return; }       // called from its mat: up at once
      if (f.at && !f.leaving && self.t - f.t0 < DWELL) return;
      self.depart(f, want);
    });
  };
  // what a frog is drawn with now: {anim, frame, flip, u, v}. A clip the station has no sheet of is `null` (housedraw.js draws a mark).
  Sim.prototype.pose = function (f) {
    var A = this.anims, t = this.t - f.t0, s = f.at ? SPOTS[f.at.room][f.at.i] : null, name, a, flip = s ? s.flip : false, frame = 0;
    if (f.st === 'walk' || !s) {
      var w = this.walk[f.dir] || {}; name = w.anim; a = A[name]; flip = !!w.flip;
      frame = a ? Math.floor(f.phase) % a.n : 0;
      if (f.st !== 'walk') { name = A.think ? 'think' : name; a = A[name]; flip = false; frame = a ? Math.floor(t * a.fps) % a.n : 0; }
    } else if (f.st === 'down' || f.st === 'up') {
      name = 'sleep_in'; a = A[name];
      frame = a ? clamp(Math.floor(f.st === 'down' ? t * a.fps : a.n - 1 - t * a.fps * UP), 0, a.n - 1) : 0;
    } else {
      name = f.st === 'sleep' ? 'sleep_loop' : ROOM[f.at.room].anim; a = A[name];
      frame = a ? Math.floor((t + (f.n % 97) / 10) * a.fps) % a.n : 0;
    }
    return { anim: a ? name : null, frame: frame, flip: flip, u: f.u, v: f.v, lying: f.st === 'sleep' || f.st === 'down' || f.st === 'up', seated: !!(s && s.desk) };
  };
  // how many are awake in each room, and who has which spot
  Sim.prototype.census = function () {
    var out = {}, self = this;
    ROOMS.forEach(function (r) { out[r.id] = { here: 0, awake: 0, coming: 0 }; });
    Object.keys(this.frogs).forEach(function (k) {
      var f = self.frogs[k];
      if (f.at) { out[f.at.room].here++; if (f.at.room !== 'bunk') out[f.at.room].awake++; }
      else if (f.goal) out[f.goal.room].coming++;
    });
    return out;
  };

  // a made-up floor that changes every reading, for looking at the house when nobody is at work (house.html?demo)
  function demo(t) {
    var beat = Math.floor(t / 20), lane = { branch: 'lane/show/frog-house', workers: [], items: [], assets: [], last: [], dirty: 0, dirty_files: [], ahead: 3, live: true };
    var other = { branch: 'lane/sim/melee', workers: [], items: [], assets: [], last: [], dirty: 0, dirty_files: [], ahead: 1, live: true };
    var skills = ['pipeline', 'tw-balance-sim', 'tw-bug-catcher', 'tw-character-sim', 'tw-critic', 'tw-destruction-vfx', 'tw-env-sim', 'tw-master', 'tw-optimizer',
                  'tw-vehicle-sim', 'tw-vfx-sheets', 'unity-pipeline'];
    var roster = skills.map(function (id) { return { id: id, kind: 'skill', name: id, does: 'a skill of the project', busy: [], home: HOME[id] }; });
    roster.push({ id: 'agent:gamedesign', kind: 'agent', name: 'gamedesign', does: 'Game design advisor', busy: [], home: 'plan', where: 'yours' });
    var S = [['The frog house page', ['work', 'work', 'lab', 'work', 'shop', 'work'], 'editing housedraw.js'],
             ['Melee balance sweep', ['lab', 'lab', 'shop', 'lab', 'plan', 'lab'], 'reading the sweep results'],
             ['Night look, part two', ['studio', 'studio', 'work', 'studio', 'studio', 'lab'], 'rendering the fire streaks'],
             ['Proving ground gate', ['shop', 'lab', 'lab', 'shop', 'work', 'shop'], 'running the gate'],
             ['Unit roster v3', ['plan', 'plan', 'work', 'plan', 'work', 'work'], 'planning the roster'],
             ['Asset gates', ['work', 'lab', 'work', 'work', 'bunk', 'bunk'], 'writing assetgate.py'],
             ['Relay budget', ['bunk', 'bunk', 'bunk', 'work', 'work', 'lab'], 'checking the legs']];
    S.forEach(function (s, i) {
      var act = s[1][(beat + i) % 6], w = { kind: 'session', id: 'session:demo000' + i, name: 'Claude', title: s[0], what: 'Demo: ' + s[0].toLowerCase(), act: act,
        state: act === 'bunk' ? 'resting' : 'working', doing: act === 'bunk' ? '' : s[2], age: act === 'bunk' ? 1500 + i * 300 : 20 };
      if (i === 4 && beat % 6 >= 3 && beat % 6 <= 4) { w.wait = 'owner'; w.doing = 'asking the owner'; }
      (i % 3 === 1 ? other : lane).workers.push(w);
    });
    function call(id, on, what) { var r = roster.filter(function (x) { return x.id === id; })[0]; if (!on) return; r.busy = [lane.branch];
      lane.workers.push({ kind: r.kind, id: id, name: r.name, what: what, state: 'working', act: r.home }); }
    call('tw-critic', beat % 6 >= 1 && beat % 6 <= 3, 'scoring round 4');
    call('tw-vfx-sheets', beat % 6 >= 2, 'a new fire flipbook');
    call('tw-master', beat % 6 >= 4, 'reviewing the lane');
    call('agent:gamedesign', beat % 6 === 2 || beat % 6 === 3, 'the shop economy');
    if (beat % 6 <= 3) for (var k = 0; k < 2; k++) lane.workers.push({ kind: 'agent', id: 'agent:Explore', uid: 'agent:Explore#demo' + k, name: 'Explore', what: k ? 'where the HUD reads the roster' : 'every caller of SimHost', state: 'working', act: 'lab' });
    lane.workers.push({ kind: 'machine', id: 'pid:4242', name: 'Unity editor', what: 'since 14:02', state: 'working', act: 'shop' });
    if (beat % 6 >= 2 && beat % 6 <= 4) other.workers.push({ kind: 'machine', id: 'pid:5151', name: 'Blender, rendering films', what: 'since 14:20', state: 'working', act: 'studio' });
    return { now: 'demo', demo: true, lanes: [lane, other], roster: roster, integration: 'demo', counts: {} };
  }

  // The part of the notice board that says something (its words, its number, the first squares), as a box on the
  // picture: [x0, y0, x1, y1] in the plan's own view. Nothing is put over it: no frog stands before it, and
  // housedraw.js keeps the names and the hall's own name off it.
  function boardFace() {
    var th = THINGS.filter(function (t) { return t.kind === 'board'; })[0], b = th.box, u0 = b[0] + 12, u1 = b[0] + 12 + (b[2] - b[0] - 24) * 0.62, z0 = 40, z1 = b[4] - 12;
    var a = iso(u0, b[3], z1), c = iso(u1, b[3], z0);
    return [a[0], a[1], c[0], c[1]];
  }
  // Where a frog's name goes on the picture, in px on the stage. Over its frog; when that place is taken, a step to
  // one side (it still stands over the frog, or begins at it), or up onto the name in the way, whichever moves it
  // least. Never more than TAG_FAR above its place: a name pushed up past the others stood 100 px from its frog and
  // said nothing about who it named, and three of them piled up over a doorway. With no place that near, null: the
  // name is left out (the list and a hover still say it).
  // cx, top: the middle and the top of the frog's box; w, h: the name's size; placed: the boxes taken, [x0, y0, x1, y1];
  // W: the stage's width. Returns [x, y, how far up from its place].
  var TAG_FAR = 24;           // px more than the name's own height: one row up, and the step between two neighbours' heads
  function tagHit(x, y, w, h, placed) {
    for (var k = 0; k < placed.length; k++) { var r = placed[k]; if (x < r[2] + 4 && x + w + 4 > r[0] && y < r[3] + 2 && y + h + 2 > r[1]) return r; }
    return null;
  }
  function tagPlace(cx, top, w, h, placed, W) {
    var base = top - h - 4, best = null;
    [0, w / 2 - 14, 14 - w / 2, w / 2 + 8, -w / 2 - 8].forEach(function (dx, i) {
      var x = clamp(cx - w / 2 + dx, 4, Math.max(4, W - w - 4)), y = base, r = tagHit(x, y, w, h, placed), n = 0;
      while (r && n++ < 4) { y = r[1] - h - 3; r = tagHit(x, y, w, h, placed); }
      if (r || y < 4 || base - y > h + TAG_FAR) return;
      var cost = base - y + [0, 4, 4, 8, 8][i];
      if (!best || cost < best[3]) best = [x, y, base - y, cost];
    });
    return best && best.slice(0, 3);
  }
  // the box a standing frog is drawn in, by where its feet are (housedraw.js draws and clicks it by the same box)
  function frogBox(u, v) { var c = iso(u, v); return [c[0] - 46, c[1] - 156, c[0] + 46, c[1] + 4]; }

  var api = { COS: COS, SIN: SIN, iso: iso, plan: plan, hash: hash, U: U, V: V, ROOMS: ROOMS, ROOM: ROOM, roomAt: roomAt, WALLS: WALLS, TALL: TALL, ALONG_U: ALONG_U, ALONG_V: ALONG_V, boardFace: boardFace, frogBox: frogBox, CLEAR: CLEAR, tagPlace: tagPlace, TAG_FAR: TAG_FAR,
    GATE: GATE, GATE_NODE: GATE_NODE, SPOTS: SPOTS, THINGS: THINGS, NODES: NODES, route: route, length: length, heading: heading, roomOf: roomOf, frogs: frogs, Sim: Sim,
    DWELL: DWELL, demo: demo, look: look, made: made, tally: tally, VENDOR: VENDOR };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.House = api;
})(typeof window !== 'undefined' ? window : this);
