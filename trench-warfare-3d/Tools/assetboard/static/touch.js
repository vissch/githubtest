// What a worker is doing and what it touches, as a graph: the worker in the middle, everything its last calls
// touched round it, each thing in the cluster of the checkout (or folder, or kind) it belongs to. The data is
// data/touch.js (src_touch.py: window.TOUCH.by[<worker>] = {nodes, links, log, counts, ...}); this file is the rules,
// with no page in it, so node runs it (test_touch.py); touchdraw.js draws what they say.
//
// The layout is made to stand still. A cluster keeps the place it was given, and a thing keeps its seat in its
// cluster (`prev`, the layout of the reading before): a reading that adds one thing adds one dot and moves nothing
// else more than a neighbour's nudge. Without a reading before, the same data gives the same picture.
(function (root) {
  'use strict';
  var CORE = 0.15, STEP = 0.040, PAD = 0.035, GAP = 0.12, GOLD = 2.399963229728653, TAU = Math.PI * 2;
  var FILES = { read: 1, changed: 1, named: 1, picture: 1, reported: 1 };
  // what a kind of node is called, one of it and its cluster when it hangs under nothing
  var WORD = { read: 'file read', changed: 'file changed', named: 'file named in a command', picture: 'picture or film', command: 'command', search: 'search',
    checkout: 'branch', folder: 'folder', subagent: 'agent it sent', skill: 'skill', web: 'the web', message: 'message', tool: 'tool',
    run: 'relay run', unit: 'unit of work', lane: 'lane', commit: 'commit', reported: 'file named in a report' };
  var LOOSE = { command: 'commands', search: 'searches', subagent: 'agents sent', skill: 'skills', web: 'the web', message: 'messages', tool: 'tools', run: 'the run', unit: 'units', files: 'loose files', folder: 'folders', checkout: 'other checkouts', lane: 'lanes', commit: 'commits' };

  // a name as a number: FNV-1a, then stirred, so that two names that differ in their last letter only are far apart
  function hash(s) {
    var h = 2166136261; s = String(s); for (var i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); }
    h ^= h >>> 16; h = Math.imul(h, 0x85ebca6b); h ^= h >>> 13; h = Math.imul(h, 0xc2b2ae35); h ^= h >>> 16;
    return h >>> 0;
  }
  function unit(s) { return hash(s) / 4294967296; }
  // the names a worker's graph may be filed under, the nearest first
  function keys(w) { return [w.uid, w.run ? 'run:' + w.run : '', w.id].filter(Boolean); }
  function entry(T, w) { var by = (T && T.by) || {}, k = keys(w || {}).filter(function (k) { return by[k]; })[0]; return k ? by[k] : null; }
  function keyOf(T, w) { var by = (T && T.by) || {}; return keys(w || {}).filter(function (k) { return by[k]; })[0] || (w && (w.uid || w.id)) || ''; }
  // whether a worker can have a graph at all: a session, an agent, a relay leg; a skill or a machine makes no calls of its own
  function has(w, e) { return !!w && (w.kind === 'session' || w.kind === 'agent') && (!!e || w.state === 'working' || w.kind === 'session'); }

  // ---- the graph of one worker: every node with the cluster it is in, and the clusters.
  // `others` is every worker's entry (TOUCH.by) and `key` this one's name in it: a file another worker touched too
  // says who (`also`), and what the others reached and this one did not is there as far clusters (`far`), drawn faint.
  function model(e, others, key) {
    var nodes = (e && e.nodes) || [], byId = {}, heads = {}, groups = {}, order = [];
    nodes.forEach(function (n) { byId[n.id] = n; });
    nodes.forEach(function (n) { if (n.p && byId[n.p]) heads[n.p] = true; });
    function group(id, label, kind, head) {
      if (!groups[id]) { groups[id] = { id: id, label: label, kind: kind, head: head || '', members: [], n: 0, last: 0, changed: 0, files: 0 }; order.push(id); }
      return groups[id];
    }
    var out = nodes.map(function (n) {
      var g, isHead = !!heads[n.id];
      if (isHead) g = group('g:' + n.id, n.label, n.kind, n.id);
      else if (n.p && byId[n.p]) g = group('g:' + n.p, byId[n.p].label, byId[n.p].kind, n.p);
      else { var k = FILES[n.kind] ? 'files' : n.kind; g = group('k:' + k, LOOSE[k] || k, k, ''); }
      var m = { id: n.id, kind: n.kind, label: n.label, full: n.full || n.label, n: n.n || 0, last: n.last || 0, g: g.id, head: isHead, t: n.t || [], m: n.m || 0 };
      ['p', 'to', 'ref', 'on', 'q', 'verdict'].forEach(function (f) { if (n[f]) m[f] = n[f]; });
      if (!isHead) { g.members.push(n.id); g.n += 1 + (n.m ? n.m - 1 : 0); if (n.kind === 'changed') g.changed++; if (FILES[n.kind] || n.kind === 'folder') g.files += n.m || 1; }
      g.last = Math.max(g.last, m.last);
      return m;
    });
    var mine = {}; out.forEach(function (n) { mine[n.id] = n; });
    var far = {};
    Object.keys(others || {}).sort().forEach(function (k) {
      if (k === key) return;
      var o = others[k], oh = {};
      (o.nodes || []).forEach(function (n) { if (mine[n.id] && (FILES[n.kind] || n.kind === 'folder')) { (mine[n.id].also = mine[n.id].also || []).push(k); } });
      (o.nodes || []).forEach(function (n) { if (n.p) oh[n.p] = (oh[n.p] || 0) + 1; });
      (o.nodes || []).forEach(function (n) {
        if (!oh[n.id] || mine[n.id] || (n.kind !== 'checkout' && n.kind !== 'folder')) return;
        var f = far[n.id] = far[n.id] || { id: 'far:' + n.id, label: n.label, kind: n.kind, head: '', members: [], n: 0, last: 0, far: true, by: [], ref: n.ref || '' };
        f.n += oh[n.id]; f.last = Math.max(f.last, n.last || 0); f.by.push(k);
      });
    });
    var fars = Object.keys(far).map(function (k) { return far[k]; }).sort(function (a, b) { return b.n - a.n || (a.id < b.id ? -1 : 1); }).slice(0, 5);
    return { nodes: out, byId: mine, groups: order.map(function (id) { return groups[id]; }).concat(fars), links: (e && e.links) || [] };
  }

  // ---- where everything stands, in a field whose middle is the worker: {pos: {node: [x, y]}, at: {cluster: [x, y]},
  // r: {cluster: radius}, slots: {cluster: {node: seat}}, box: [x0, y0, x1, y1]}. `aspect` is how wide the picture is
  // for its height: a wide one spreads the clusters sideways.
  function seatAt(c, gid, i, step) { var a = i * GOLD + unit(gid + '/turn') * TAU, d = step * Math.sqrt(i + 1.6); return [c[0] + Math.cos(a) * d, c[1] + Math.sin(a) * d]; }
  function layout(m, prev, aspect) {
    prev = prev || {}; var ax = Math.max(0.7, Math.min(2.4, Math.pow(aspect || 1, 0.75)));
    var slots = {}, at = {}, r = {}, pos = {}, gs = m.groups, step = prev.step || STEP * (m.nodes.length < 14 ? 2 : m.nodes.length < 30 ? 1.45 : 1);
    gs.forEach(function (g) {
      var was = (prev.slots || {})[g.id] || {}, mine = {}, used = {}, top = -1;
      g.members.forEach(function (id) { if (was[id] != null && !used[was[id]]) { mine[id] = was[id]; used[was[id]] = true; } });
      var free = 0;
      g.members.forEach(function (id) { if (mine[id] != null) return; while (used[free]) free++; mine[id] = free; used[free] = true; });
      Object.keys(mine).forEach(function (id) { top = Math.max(top, mine[id]); });
      slots[g.id] = mine;
      r[g.id] = g.far ? 0.13 + 0.016 * Math.sqrt(Math.min(g.n, 80)) : Math.min(0.62, step * Math.sqrt(top + 2.6) + PAD);
      var had = (prev.at || {})[g.id];
      if (had) at[g.id] = [had[0], had[1]];
      else if (!g.far) { var a = unit(g.id) * TAU, d = CORE + r[g.id] + GAP + 0.3 * unit(g.id + '/out'); at[g.id] = [Math.cos(a) * d * ax, Math.sin(a) * d]; }
    });
    // clusters that overlap push each other apart, and none lies over the worker; the same order every time
    function settle(gs) {
     for (var it = 0; it < 160; it++) {
      var moved = false;
      for (var i = 0; i < gs.length; i++) {
        var A = gs[i], pa = at[A.id];
        for (var j = i + 1; j < gs.length; j++) {
          var B = gs[j], pb = at[B.id], dx = pb[0] - pa[0], dy = pb[1] - pa[1], d = Math.sqrt(dx * dx + dy * dy), need = r[A.id] + r[B.id] + GAP;
          if (d >= need) continue;
          if (d < 1e-6) { var t = unit(A.id + B.id) * TAU; dx = Math.cos(t); dy = Math.sin(t); d = 1; }
          if ((A.far || B.far) && d >= need - GAP) continue;
          var push = (need - d) / 2 + 1e-4, wa = A.far === B.far ? 0.5 : A.far ? 1 : 0;      // a far cluster gives way to one the worker reached
          pa[0] -= dx / d * push * 2 * wa; pa[1] -= dy / d * push * 2 * wa; pb[0] += dx / d * push * 2 * (1 - wa); pb[1] += dy / d * push * 2 * (1 - wa); moved = true;
        }
        var d0 = Math.sqrt(pa[0] * pa[0] + pa[1] * pa[1]), least = CORE + r[A.id] + GAP;
        if (d0 < least) { if (d0 < 1e-6) { var t0 = unit(A.id) * TAU; pa[0] = Math.cos(t0) * least; pa[1] = Math.sin(t0) * least; } else { pa[0] *= least / d0; pa[1] *= least / d0; } moved = true; }
      }
      if (!moved) break;
     }
    }
    var near = gs.filter(function (g) { return !g.far; }), reach = CORE + 0.25;
    settle(near);
    near.forEach(function (g) { var c = at[g.id]; reach = Math.max(reach, Math.sqrt(c[0] * c[0] / (ax * ax) + c[1] * c[1]) + r[g.id]); });
    // what the worker did not reach lies just outside what it did
    var fars = gs.filter(function (g) { return g.far; }).sort(function (a, b) { return a.id < b.id ? -1 : 1; });
    fars.forEach(function (g, i) { if (at[g.id]) return; var a = 0.55 + (i + 0.3 * unit(g.id)) * TAU / fars.length, d = reach + r[g.id] * 0.8 + 0.1 * unit(g.id + '/far'); at[g.id] = [Math.cos(a) * d * ax, Math.sin(a) * d]; });
    settle(gs);
    var box = [-CORE, -CORE, CORE, CORE];
    gs.forEach(function (g) {
      var c = at[g.id], rr = r[g.id] * (g.far ? 0.25 : 1);
      box = [Math.min(box[0], c[0] - rr), Math.min(box[1], c[1] - rr), Math.max(box[2], c[0] + rr), Math.max(box[3], c[1] + rr)];
      if (g.head) pos[g.head] = [c[0], c[1]];
      g.members.forEach(function (id) { pos[id] = seatAt(c, g.id, slots[g.id][id], step); });
    });
    return { pos: pos, at: at, r: r, slots: slots, box: box, step: step };
  }
  // the faint points of a cluster: what a folded folder holds, what others touched in a cluster this worker did not reach
  function cloud(gid, c, rad, n) {
    var out = []; n = Math.min(n, 110);
    for (var i = 0; i < n; i++) { var a = unit(gid + '#a' + i) * TAU, d = Math.pow(unit(gid + '#d' + i), 0.8) * rad; out.push([c[0] + Math.cos(a) * d, c[1] + Math.sin(a) * d, 0.25 + 0.75 * unit(gid + '#b' + i)]); }
    return out;
  }

  // ---- words and numbers
  function two(n) { return (n < 10 ? '0' : '') + n; }
  function clock(at, secs) { var d = new Date(at * 1000); return two(d.getHours()) + ':' + two(d.getMinutes()) + (secs ? ':' + two(d.getSeconds()) : ''); }
  function ago(sec) { sec = Math.max(0, Math.round(sec)); return sec < 60 ? sec + ' s' : sec < 3600 ? Math.round(sec / 60) + ' min' : sec < 172800 ? Math.round(sec / 3600) + ' h' : Math.round(sec / 86400) + ' d'; }
  // how lately a thing was touched, 1 (now) to 0: against the worker's own last call when it is over, so its graph does not go dark
  function fresh(last, ref, half) { return Math.pow(0.5, Math.max(0, ref - last) / (half || 600)); }
  // the things the tentacles reach for: the ones touched within `span` seconds of the reference, at most `most`; and always the
  // `least` touched last, however long ago (a worker that waits on its agents still holds what it last had)
  function reach(m, ref, span, most, least) {
    return m.nodes.filter(function (n) { return !n.head; }).sort(function (a, b) { return b.last - a.last || (a.id < b.id ? -1 : 1); })
      .filter(function (n, i) { return i < (least || 0) || ref - n.last <= span; }).slice(0, most).map(function (n) { return n.id; });
  }
  // how a worker stands, from the floor and from its own last call: at work with a call in the last three minutes it is live; at
  // work and quiet for longer (it waits on its agents, or thinks) it is quiet, and says for how long; else it rests, or is over
  function state(w, e, now) {
    var working = !!w && w.state === 'working', age = e && e.last ? Math.max(0, now - e.last) : null;
    if (e && e.gone) return { id: 'over', words: 'over' };
    if (e && e.kind === 'run') return working ? { id: 'live', words: 'running' } : { id: 'resting', words: 'ended' };
    if (!working) return { id: 'resting', words: 'resting' };
    if (age === null || age < 180) return { id: 'live', words: 'live' };
    return { id: 'quiet', words: 'quiet for ' + ago(age) };
  }
  // what it is doing now, in words: its last call, how long ago, and the node it touched
  function doing(e, now) { var l = e && e.log && e.log.length ? e.log[e.log.length - 1] : null; return l ? { text: l.verb + ' ' + l.thing, age: l.now ? 0 : Math.max(0, now - l.at), id: l.id || '' } : null; }
  // ... and when, in words: now, or how long ago; a worker that is quiet says that once, not twice
  function since(d, st) { return d.age < 90 && st.id === 'live' ? 'now' : st.id === 'quiet' ? st.words : ago(d.age) + ' ago' + (st.id === 'live' ? '' : ' \u00b7 ' + st.words); }
  // the heat strip as the hour before now: the one in the data ends on the worker's last call, so it moves left by the time since
  function hour(heat, last, now, span) {
    var k = Math.max(0, Math.floor((now - last) / (span || 120)));
    return (heat || []).map(function (row) { var out = row.slice(Math.min(k, row.length)); while (out.length < row.length) out.push(0); return out; });
  }
  // how many names lie on another name, or on a dot that is not their own: names [x0, y0, x1, y1, id], dots {id: [x, y, r]}
  function overlaps(names, dots) {
    var n = 0;
    names.forEach(function (a, i) {
      names.forEach(function (b, j) { if (j > i && a[0] < b[2] && a[2] > b[0] && a[1] < b[3] && a[3] > b[1]) n++; });
      Object.keys(dots).forEach(function (id) { var d = dots[id]; if (id !== a[4] && id !== 'core' && d[0] + d[2] > a[0] && d[0] - d[2] < a[2] && d[1] + d[2] > a[1] && d[1] - d[2] < a[3]) n++; });
    });
    return n;
  }
  // what the page is redrawn on
  function sig(e) { return e ? [e.last || 0, e.calls || 0, (e.nodes || []).length, (e.log || []).length, e.gone ? 1 : 0] : null; }
  // every touch of a thing the data still has: its own marks, and the lines of the log that name it; newest first
  function touches(e, n) {
    var seen = {}, out = [];
    (n.t || []).forEach(function (t) { seen[t[0] + '|' + t[1]] = true; out.push({ at: t[0], verb: t[1] }); });
    ((e && e.log) || []).forEach(function (l) { if (l.id === n.id && !seen[l.at + '|' + l.verb]) { seen[l.at + '|' + l.verb] = true; out.push({ at: l.at, verb: l.verb }); } });
    return out.sort(function (a, b) { return b.at - a.at; });
  }
  // the calls a minute over the last ten minutes of the heat strip (30 buckets of two minutes)
  function rate(e) { var h = (e && e.heat) || [], n = 0; h.forEach(function (row) { for (var i = row.length - 5; i < row.length; i++) n += row[i] || 0; }); return Math.round(n / 10 * 10) / 10; }
  // the node under a point of the picture: `pts` is {id: [x, y, radius]} in the picture's own pixels
  function pick(pts, x, y, slack) {
    var best = '', bd = Infinity;
    Object.keys(pts).forEach(function (id) { var p = pts[id], d = Math.sqrt((p[0] - x) * (p[0] - x) + (p[1] - y) * (p[1] - y)) - (p[2] || 0); if (d <= (slack || 6) && d < bd) { bd = d; best = id; } });
    return best;
  }
  // what a click on a node does: an agent it sent is opened, a branch goes to its room, a run to the runs, anything else is shown
  function action(n) {
    if (n.kind === 'subagent') return n.to ? { go: 'worker', key: n.to } : { go: 'show' };
    if ((n.kind === 'checkout' || n.kind === 'lane') && n.ref && n.ref.indexOf('detached') !== 0) return { go: 'page', href: 'floor.html#room-' + String(n.ref).replace(/[^a-z0-9]+/gi, '-') };
    if (n.kind === 'run') return { go: 'page', href: 'runs.html' + (n.ref ? '#run-' + n.ref : '') };
    return { go: 'show' };
  }
  // a file's whole path as the owner reads it: the checkout it is in, then the path
  function path(m, n) { var h = n.p && m.byId[n.p]; return h && h.kind === 'checkout' && FILES[n.kind] || h && h.kind === 'checkout' && n.kind === 'folder' ? h.full.split(' · ').pop() + '/' + n.full : n.full; }

  var pure = { CORE: CORE, WORD: WORD, FILES: FILES, hash: hash, unit: unit, keys: keys, entry: entry, keyOf: keyOf, has: has, model: model, layout: layout, cloud: cloud, clock: clock, ago: ago,
    fresh: fresh, reach: reach, state: state, doing: doing, since: since, hour: hour, overlaps: overlaps, sig: sig, touches: touches, rate: rate, pick: pick, action: action, path: path };
  if (typeof module !== 'undefined' && module.exports) module.exports = pure; else root.Touch = pure;
})(typeof window !== 'undefined' ? window : this);
