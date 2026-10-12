// What a worker is doing and what it touches, drawn: the worker in the middle of a dark field, dotted tentacles out
// to what its last calls touched, each thing a dot in the cluster of its checkout or its kind, and faint clouds for
// what the others on the floor reached and this one did not. (The owner, 2026-10-12: "when clicking on an agent in
// the house we can see a data visualization like this on what the agent is exactly doing and what it touches".)
// Two places: small in the profile beside the house (board.js gives it the part b-touch, with the counters and the
// last calls), and, on a click, full size over the house and the profile, with the running log, the counters, the
// clusters and the heat of the last hour beside it. Escape or Close goes back.
// A click on a node: an agent it sent opens that agent (and picks its frog in the house), a branch goes to its room
// on floor.html, a run to runs.html, anything else says its whole path and every time it was touched.
// The data is data/touch.js, read again with the floor every 20 seconds (crew.js); the rules are touch.js.
// The picture draws itself thirty times a second while it is on screen and something moves, a few times a second
// when nothing does, and not at all off screen. With prefers-reduced-motion it is drawn once per reading, settled.
(function () {
  'use strict';
  var T = window.Touch;
  if (!T || !document.createElement('canvas').getContext) return;
  var C = window.Crew, ROOT = document.body.dataset.root || '';
  var still = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var MONO = '"Roboto Mono", Consolas, "Courier New", monospace', SANS = 'Inter, "Segoe UI", sans-serif', TAU = Math.PI * 2;
  var COL = { read: '#5b9dff', changed: '#ff6a3d', named: '#8ea2c8', picture: '#b98cff', command: '#34d399', search: '#56d4e0', checkout: '#e5e7ee', folder: '#8ea2c8',
    subagent: '#ff7eb6', skill: '#ffd166', web: '#2dd4bf', message: '#f2f3f6', tool: '#a7aebf', run: '#f2f3f6', unit: '#ffb03a', lane: '#5b9dff', commit: '#34d399', reported: '#8ea2c8', files: '#8ea2c8' };
  var VERB = { read: 'read', 'looked at': 'picture', edited: 'changed', wrote: 'changed', ran: 'command', searched: 'search', sent: 'subagent', messaged: 'message', asked: 'message',
    'took up': 'skill', running: 'unit', fetched: 'web', 'searched the web': 'web', called: 'tool', commit: 'commit', committed: 'commit' };
  var memory = {};          // a picture's own state, kept while the page lives: where everything stood, so a part made again does not start over
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function by() { return (window.TOUCH && window.TOUCH.by) || {}; }
  function clockNow() { return Date.now() / 1000; }
  function rgba(hex, a) { var n = parseInt(hex.slice(1), 16); return 'rgba(' + (n >> 16) + ',' + ((n >> 8) & 255) + ',' + (n & 255) + ',' + a + ')'; }
  function workerOf(key) {
    var o = window.OPS, hit = null;
    ((o && o.lanes) || []).forEach(function (l) { l.workers.forEach(function (w) { if (!hit && (w.uid || w.id) === key) hit = w; }); });
    return hit;
  }
  function titleOf(key) {
    var w = workerOf(key), e = by()[key];
    return w && C ? C.title(w) : (e && e.title) || String(key).replace(/^(agent|session|leg):/, '').replace(/#.*/, '');
  }
  // the moment freshness is told against: now, or the worker's own last call once that is long ago (its graph does not go dark)
  function refOf(e) { var n = clockNow(); return e && e.last && n - e.last > 1200 ? e.last : n; }

  // ---- one picture on one canvas. opt: {mini, onpick(node or null), onhover(node or null, x, y)}
  function View(cv, opt) {
    var ctx = cv.getContext('2d'), W = 0, H = 0, dpr = 1, key = '', e = null, w = null, m = null, L = null, mem = null, pts = {}, hover = '', chosen = '', lit = '';
    var hits = [], written = [], overG = '';          // where the clusters' names are (a click on one lights it), every name written, the cluster under the pointer
    var seen = true, dead = false, raf = 0, last = performance.now(), drawn = 0, cam = null, clouds = {}, sky = null, busyUntil = 0, self = {};
    function size() {
      var r = cv.getBoundingClientRect(); if (!r.width || !r.height) return;
      dpr = Math.min(2, window.devicePixelRatio || 1); W = Math.round(r.width); H = Math.round(r.height);
      cv.width = Math.round(W * dpr); cv.height = Math.round(H * dpr); sky = null; cam = null; arrange(); kick();
    }
    function arrange() {
      if (!m || !W) return;
      L = T.layout(m, mem.prev, W / H); mem.prev = { at: L.at, slots: L.slots, step: L.step };
      var fresh = !mem.began; mem.began = true;
      m.nodes.forEach(function (n) { if (!mem.shown[n.id]) mem.shown[n.id] = still || (fresh && opt.mini) ? L.pos[n.id].slice() : [0, 0]; });      // a new thing comes out of the worker
      Object.keys(mem.shown).forEach(function (id) { if (!L.pos[id]) delete mem.shown[id]; });
      m.groups.forEach(function (g) { if (!mem.gs[g.id]) mem.gs[g.id] = still || (fresh && opt.mini) ? L.at[g.id].slice() : [L.at[g.id][0] * 0.3, L.at[g.id][1] * 0.3]; });
    }
    function set(k, entry, worker) {
      key = k; e = entry; w = worker || null;
      var name = (opt.mini ? 'm|' : 'f|') + k; mem = memory[name] = memory[name] || { prev: null, shown: {}, gs: {}, began: false };
      m = T.model(e, by(), k); clouds = {};
      if (chosen && !m.byId[chosen]) { chosen = ''; if (opt.onpick) opt.onpick(null); }
      if (lit && !m.groups.some(function (g) { return g.id === lit; })) lit = '';
      arrange(); kick();
    }
    function kick() { busyUntil = performance.now() + 2500; if (still && m && L && W) { step(1); render(performance.now()); } }
    function aim() {
      var b = L.box, mx = opt.mini ? 14 : Math.max(40, Math.min(110, W * 0.07)), my = opt.mini ? 24 : Math.max(40, Math.min(60, H * 0.07));
      var s = Math.min((W - 2 * mx) / (b[2] - b[0]), (H - 2 * my) / (b[3] - b[1]), Math.min(W, H) * (opt.mini ? 1.5 : m.nodes.length < 20 ? 1.9 : 1.15));
      return { s: s, x: W / 2 - (b[0] + b[2]) / 2 * s, y: H / 2 - (b[1] + b[3]) / 2 * s };
    }
    function S(p) { return [cam.x + p[0] * cam.s, cam.y + p[1] * cam.s]; }
    function step(dt) {
      var k = still ? 1 : 1 - Math.exp(-dt * 5.5), moving = false, a = aim();
      if (!cam || still) cam = a; else { cam.s += (a.s - cam.s) * k; cam.x += (a.x - cam.x) * k; cam.y += (a.y - cam.y) * k; }
      function ease(have, want) { var dx = want[0] - have[0], dy = want[1] - have[1]; if (Math.abs(dx) + Math.abs(dy) > 0.0015) moving = true; have[0] += dx * k; have[1] += dy * k; }
      m.nodes.forEach(function (n) { ease(mem.shown[n.id], L.pos[n.id]); });
      m.groups.forEach(function (g) { ease(mem.gs[g.id], L.at[g.id]); });
      return moving;
    }
    // the field behind everything: a little light round the worker and a scatter of far points, drawn once per size
    function backdrop() {
      if (sky) return sky;
      sky = document.createElement('canvas'); sky.width = cv.width; sky.height = cv.height;
      var c = sky.getContext('2d'); c.scale(dpr, dpr);
      c.fillStyle = '#0d111b'; c.fillRect(0, 0, W, H);
      var g = c.createRadialGradient(W / 2, H / 2, 0, W / 2, H / 2, Math.max(W, H) * 0.62);
      g.addColorStop(0, 'rgba(52, 68, 112, .42)'); g.addColorStop(0.55, 'rgba(26, 34, 58, .30)'); g.addColorStop(1, 'rgba(13, 17, 27, 0)');
      c.fillStyle = g; c.fillRect(0, 0, W, H);
      var n = Math.round(W * H / (opt.mini ? 900 : 1500));
      for (var i = 0; i < n; i++) { var u = T.unit('sky' + i), v = T.unit('skz' + i), b = T.unit('skb' + i); c.fillStyle = 'rgba(190, 205, 245,' + (0.04 + 0.16 * b * b).toFixed(3) + ')'; c.fillRect(u * W, v * H, b > 0.93 ? 1.6 : 1, b > 0.93 ? 1.6 : 1); }
      return sky;
    }
    function shape(n, x, y, r, col, a) {
      ctx.globalAlpha = a; ctx.fillStyle = col; ctx.strokeStyle = col; ctx.lineWidth = 1.4;
      var k = n.kind;
      if (n.m > 1) {          // older things folded together: a pinch of points
        for (var i = 0; i < Math.min(n.m, 14); i++) { var t = T.unit(n.id + 'a' + i) * TAU, d = 3 + T.unit(n.id + 'd' + i) * (5 + r); ctx.fillRect(x + Math.cos(t) * d - 0.8, y + Math.sin(t) * d - 0.8, 1.6, 1.6); }
        ctx.strokeRect(x - 2.5, y - 2.5, 5, 5); ctx.globalAlpha = 1; return;
      }
      if (k === 'command' || k === 'commit') ctx.fillRect(x - r * 0.85, y - r * 0.85, r * 1.7, r * 1.7);
      else if (k === 'search' || k === 'lane') { ctx.beginPath(); ctx.moveTo(x, y - r * 1.2); ctx.lineTo(x + r * 1.2, y); ctx.lineTo(x, y + r * 1.2); ctx.lineTo(x - r * 1.2, y); ctx.closePath(); ctx.fill(); }
      else if (k === 'skill') { ctx.beginPath(); ctx.moveTo(x, y - r * 1.25); ctx.lineTo(x + r * 1.15, y + r * 0.9); ctx.lineTo(x - r * 1.15, y + r * 0.9); ctx.closePath(); ctx.fill(); }
      else if (k === 'named' || k === 'reported' || k === 'web' || (k === 'unit' && n.q)) { ctx.beginPath(); ctx.arc(x, y, r, 0, TAU); ctx.stroke(); }
      else { ctx.beginPath(); ctx.arc(x, y, r, 0, TAU); ctx.fill(); }
      if (k === 'changed' || k === 'subagent' || k === 'run' || k === 'unit') { ctx.globalAlpha = a * 0.55; ctx.beginPath(); ctx.arc(x, y, r + 3, 0, TAU); ctx.stroke(); }
      if (k === 'picture') { ctx.fillStyle = '#0d111b'; ctx.globalAlpha = a; ctx.fillRect(x - 1, y - 1, 2, 2); }
      ctx.globalAlpha = 1;
    }
    function colour(n) { return n.kind === 'unit' && n.verdict ? (n.verdict === 'PASS' ? '#34d399' : n.verdict === 'BLOCKED' ? '#ff5a4f' : COL.unit) : COL[n.kind] || '#a7aebf'; }
    // a dotted line from a to b that bends: the worker's tentacle
    function tentacle(a, b, bend, col, alpha, phase, gap, dot) {
      var mx = (a[0] + b[0]) / 2, my = (a[1] + b[1]) / 2, dx = b[0] - a[0], dy = b[1] - a[1], len = Math.sqrt(dx * dx + dy * dy) || 1;
      var cx = mx - dy / len * bend * len, cy = my + dx / len * bend * len, n = Math.max(2, Math.floor(len * 1.08 / gap));
      ctx.strokeStyle = col; ctx.lineCap = 'round'; ctx.globalAlpha = alpha * 0.1; ctx.lineWidth = dot * 2.6; ctx.beginPath(); ctx.moveTo(a[0], a[1]); ctx.quadraticCurveTo(cx, cy, b[0], b[1]); ctx.stroke();
      ctx.fillStyle = col;
      for (var i = 0; i <= n; i++) {
        var t = (i + phase) / n; if (t > 1) continue;
        var u = 1 - t, x = u * u * a[0] + 2 * u * t * cx + t * t * b[0], y = u * u * a[1] + 2 * u * t * cy + t * t * b[1];
        ctx.globalAlpha = alpha * (0.5 + 0.5 * t); var s = dot * (0.8 + 0.5 * t); ctx.fillRect(x - s / 2, y - s / 2, s, s);
      }
      ctx.globalAlpha = 1;
    }
    function hair(a, b, bend, col, alpha) {
      var mx = (a[0] + b[0]) / 2, my = (a[1] + b[1]) / 2, dx = b[0] - a[0], dy = b[1] - a[1];
      ctx.strokeStyle = col; ctx.globalAlpha = alpha; ctx.lineWidth = 1; ctx.beginPath(); ctx.moveTo(a[0], a[1]); ctx.quadraticCurveTo(mx - dy * bend, my + dx * bend, b[0], b[1]); ctx.stroke(); ctx.globalAlpha = 1;
    }
    function render(ts) {
      if (!W || !m || !L) return;
      var t = ts / 1000, ref = refOf(e), nowS = clockNow(), work = !!w && w.state === 'working';
      if (!cam) cam = aim();
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0); ctx.drawImage(backdrop(), 0, 0, W, H);
      var core = [cam.x, cam.y], cr = Math.max(opt.mini ? 9 : 16, Math.min(opt.mini ? 20 : 46, T.CORE * 0.6 * cam.s));
      var hue = C && w ? C.hue(w.id || w.name || '') : 205, ink = 'hsl(' + hue + ', 80%, 72%)';
      var reach = {}, order = T.reach(m, ref, opt.mini ? 600 : 900, opt.mini ? 8 : 18, opt.mini ? 4 : 10); order.forEach(function (id, i) { reach[id] = i + 1; });
      pts = {};
      // ---- the clusters: what was not reached as a faint cloud, what was as a soft ground with its name
      m.groups.forEach(function (g) {
        var c = S(mem.gs[g.id]), r = L.r[g.id] * cam.s, dim = lit && lit !== g.id ? 0.35 : 1;
        if (g.far) {
          var pc = clouds[g.id] = clouds[g.id] || T.cloud(g.id, [0, 0], 1, 46 + g.n * 2);
          ctx.fillStyle = '#b8c6ee';
          pc.forEach(function (p, i) { var tw = still ? 1 : 0.75 + 0.25 * Math.sin(t * 0.9 + i); ctx.globalAlpha = (0.18 + 0.42 * p[2] * tw) * (overG === g.id ? 1.5 : 1); ctx.fillRect(c[0] + p[0] * r, c[1] + p[1] * r, p[2] > 0.8 ? 2.4 : 1.5, p[2] > 0.8 ? 2.4 : 1.5); });
          ctx.globalAlpha = 1; return;
        }
        var gr = ctx.createRadialGradient(c[0], c[1], 0, c[0], c[1], r);
        gr.addColorStop(0, rgba(COL[g.kind] || '#8ea2c8', 0.10 * dim)); gr.addColorStop(1, rgba(COL[g.kind] || '#8ea2c8', 0));
        ctx.fillStyle = gr; ctx.beginPath(); ctx.arc(c[0], c[1], r, 0, TAU); ctx.fill();
        if (lit === g.id) { ctx.strokeStyle = 'rgba(160,178,235,.5)'; ctx.setLineDash([3, 5]); ctx.lineWidth = 1; ctx.beginPath(); ctx.arc(c[0], c[1], r, 0, TAU); ctx.stroke(); ctx.setLineDash([]); }
      });
      // ---- the web inside a cluster, and the chain of a run
      var chain = e && e.kind === 'run';
      m.nodes.forEach(function (n) {
        if (n.head) return;
        var p = S(mem.shown[n.id]), g = mem.gs[n.g], dim = lit && lit !== n.g ? 0.3 : 1;
        if (!chain) hair(S(g), p, 0.12 * (T.unit(n.id) - 0.5), COL[n.kind] || '#8ea2c8', (reach[n.id] ? 0.22 : 0.09) * dim);
      });
      if (chain) m.links.forEach(function (l) { var a = l.a === 'core' ? [0, 0] : mem.shown[l.a], b = mem.shown[l.b]; if (a && b) hair(S(a), S(b), 0.1 * (T.unit(l.b) - 0.5), COL[(m.byId[l.b] || {}).kind] || '#8ea2c8', 0.34); });
      // ---- the tentacles: a thin one to every cluster, a dotted one to each thing touched lately, the newest running
      m.groups.forEach(function (g) { if (!g.far && !chain) hair(core, S(mem.gs[g.id]), 0.22 * (T.unit(g.id + 'h') - 0.5), ink, 0.16 * (lit && lit !== g.id ? 0.4 : 1)); });
      order.slice().reverse().forEach(function (id) {
        var n = m.byId[id], age = ref - n.last, f = T.fresh(n.last, ref, 300), live = work && nowS - n.last < 150 && !still;
        tentacle(core, S(mem.shown[id]), 0.5 * (T.unit(id + 'b') - 0.5), n.kind === 'changed' ? '#ffc9a3' : n.kind === 'command' ? '#b6f5dc' : n.kind === 'subagent' ? '#ffc4de' : '#fff6dc', (0.5 + 0.5 * f) * (lit && lit !== n.g ? 0.3 : 1), live ? (t * 1.1) % 1 : 0, opt.mini ? 6 : 8, (opt.mini ? 1.9 : 2.8) + (age < 150 ? 0.7 : 0) + (id === order[0] ? 0.6 : 0));
      });
      // ---- the things
      var labels = [];
      m.nodes.forEach(function (n) {
        var p = S(mem.shown[n.id]), f = T.fresh(n.last, ref, 900), dim = lit && lit !== n.g ? 0.28 : 1;
        var r = n.head ? (opt.mini ? 2.4 : 3.2) : Math.min(opt.mini ? 4.2 : 6.5, (opt.mini ? 1.7 : 2.4) + Math.sqrt(n.n) * (opt.mini ? 0.55 : 0.8)) * (n.kind === 'subagent' || n.kind === 'unit' || n.kind === 'run' ? 1.35 : 1);
        var a = (n.q ? 0.4 : 0.34 + 0.66 * f) * dim, col = colour(n);
        if (!n.head) shape(n, p[0], p[1], r, col, a); else { ctx.globalAlpha = 0.5 * dim; ctx.fillStyle = col; ctx.beginPath(); ctx.arc(p[0], p[1], r, 0, TAU); ctx.fill(); ctx.globalAlpha = 1; }
        if (n.also && !n.head) { ctx.strokeStyle = '#ffd166'; ctx.globalAlpha = 0.5 * dim; ctx.lineWidth = 1; ctx.setLineDash([2, 3]); ctx.beginPath(); ctx.arc(p[0], p[1], r + 5, 0, TAU); ctx.stroke(); ctx.setLineDash([]); ctx.globalAlpha = 1; }
        var young = nowS - n.last;
        if ((young < 50 && work || n.on) && !n.head) {          // touched just now: a ring that widens
          var ph = still ? 0.4 : (t * 0.8 + T.unit(n.id)) % 1; ctx.strokeStyle = col; ctx.globalAlpha = (1 - ph) * 0.8; ctx.lineWidth = 1.5; ctx.beginPath(); ctx.arc(p[0], p[1], r + 3 + ph * 14, 0, TAU); ctx.stroke(); ctx.globalAlpha = 1;
        }
        if (n.id === chosen || n.id === hover) { ctx.strokeStyle = '#fff'; ctx.lineWidth = 1.5; ctx.beginPath(); ctx.arc(p[0], p[1], r + 4.5, 0, TAU); ctx.stroke(); }
        if (!opt.mini && !n.head && n.m < 2) { ctx.strokeStyle = col; ctx.globalAlpha = 0.22 * dim; ctx.lineWidth = 1; ctx.beginPath(); ctx.arc(p[0], p[1], r + 2.2, 0, TAU); ctx.stroke(); ctx.globalAlpha = 1; }
        pts[n.id] = [p[0], p[1], r];
        if (!n.head && (reach[n.id] || n.id === chosen || n.id === hover || chain || (n.kind === 'subagent' && !opt.mini))) labels.push({ n: n, p: p, r: r, rank: n.id === chosen || n.id === hover ? 0 : n.kind === 'subagent' ? 0.5 : reach[n.id] || 99, col: col });
      });
      // ---- the worker
      var beat = still || !work ? 0.5 : 0.5 + 0.5 * Math.sin(t * 2.4);
      var halo = ctx.createRadialGradient(core[0], core[1], cr * 0.4, core[0], core[1], cr * 3.1);
      halo.addColorStop(0, 'hsla(' + hue + ', 85%, 70%, ' + (0.20 + 0.10 * beat) + ')'); halo.addColorStop(1, 'hsla(' + hue + ', 85%, 70%, 0)');
      ctx.fillStyle = halo; ctx.beginPath(); ctx.arc(core[0], core[1], cr * 3.1, 0, TAU); ctx.fill();
      ctx.save(); ctx.beginPath(); ctx.ellipse(core[0], core[1], cr * 1.22, cr * 0.86, 0, 0, TAU); ctx.fillStyle = '#101726'; ctx.fill(); ctx.clip();
      ctx.strokeStyle = 'hsla(' + hue + ', 70%, 70%, .42)'; ctx.lineWidth = 1;
      for (var q = -3; q <= 3; q++) { ctx.beginPath(); ctx.ellipse(core[0], core[1], Math.abs(q) * cr * 0.4 + 0.5, cr * 0.86, 0, 0, TAU); ctx.stroke(); ctx.beginPath(); ctx.moveTo(core[0] - cr * 1.3, core[1] + q * cr * 0.27); ctx.lineTo(core[0] + cr * 1.3, core[1] + q * cr * 0.27); ctx.stroke(); }
      ctx.restore();
      ctx.strokeStyle = ink; ctx.lineWidth = opt.mini ? 1.4 : 2; ctx.beginPath(); ctx.ellipse(core[0], core[1], cr * 1.22, cr * 0.86, 0, 0, TAU); ctx.stroke();
      var d = cr * (0.28 + 0.08 * beat); ctx.fillStyle = work ? '#ff5a6e' : '#8ea2c8'; ctx.beginPath(); ctx.moveTo(core[0], core[1] - d); ctx.lineTo(core[0] + d, core[1]); ctx.lineTo(core[0], core[1] + d); ctx.lineTo(core[0] - d, core[1]); ctx.closePath(); ctx.fill();
      pts['core'] = [core[0], core[1], cr];
      // ---- the names. Nothing written lies on a dot, on another name or on the worker: a name looks for a free place round its
      // thing, further out when the near ones are taken (a thin line then leads back to it), and is left out when there is none.
      var taken = [[core[0] - cr * 1.4, core[1] - cr, core[0] + cr * 1.4, core[1] + cr], [0, 0, W, W < 520 && !opt.mini ? 48 : 30], [0, H - (opt.mini ? 0 : W < 520 ? 46 : 30), W, H]];      // the worker, the line over the picture, the key under it
      written = []; hits = [];
      function free(b) { if (b[0] < 2 || b[1] < 2 || b[2] > W - 2 || b[3] > H - 2) return false; return !taken.some(function (o) { return b[0] - 3 < o[2] && b[2] + 3 > o[0] && b[1] - 3 < o[3] && b[3] + 3 > o[1]; }); }
      // a free place for a box of tw x h round the point c, `gap` away from it: first on the side `turn` points to, then round, then further out
      function place(c, gap, tw, h, turn, steps, reach) {
        for (var d = 0; d < reach.length; d++) for (var k = 0; k < steps.length; k++) {
          var a = turn + steps[k], far = gap + reach[d] + Math.abs(Math.cos(a)) * tw / 2 + Math.abs(Math.sin(a)) * h / 2;
          var x = c[0] + Math.cos(a) * far - tw / 2, y = c[1] + Math.sin(a) * far - h / 2, b = [x, y, x + tw, y + h];
          if (free(b)) return { x: x, y: y, b: b, led: d > 0 };
        }
        return null;
      }
      function lead(from, b, col) { var x = Math.max(b[0], Math.min(b[2], from[0])), y = Math.max(b[1], Math.min(b[3], from[1])); ctx.strokeStyle = col; ctx.lineWidth = 1; ctx.beginPath(); ctx.moveTo(from[0], from[1]); ctx.lineTo(x, y); ctx.stroke(); }
      ctx.textBaseline = 'middle'; ctx.textAlign = 'left';
      Object.keys(pts).forEach(function (id) { var q = pts[id]; if (id !== 'core') taken.push([q[0] - q[2] - 1, q[1] - q[2] - 1, q[0] + q[2] + 1, q[1] + q[2] + 1]); });
      // what it is doing now, under the worker: after the worker itself the loudest thing on the picture
      var tight = W < 520, doing = T.doing(e, nowS), stand = T.state(w, e, nowS);
      if (doing) {
        var f1 = opt.mini ? 10.5 : tight ? 12.5 : 15, f2 = opt.mini ? 0 : 10.5, cut = opt.mini ? 24 : tight ? 30 : 38;
        var line = doing.text.length > cut ? doing.text.slice(0, cut - 1) + '\u2026' : doing.text, fresh = doing.age < 90 && stand.id === 'live';
        var when = T.since(doing, stand);
        if (opt.mini) line += ' \u00b7 ' + (fresh ? 'now' : T.ago(doing.age));
        ctx.font = '600 ' + f1 + 'px ' + SANS; var nw = ctx.measureText(line).width; if (f2) { ctx.font = f2 + 'px ' + MONO; nw = Math.max(nw, ctx.measureText(when).width); }
        var nh = f1 + (f2 ? f2 + 5 : 0) + 8, nx = Math.max(3, Math.min(W - nw - 19, core[0] - nw / 2 - 8)), under = core[1] + cr + 5, ny = under, nb = [nx, ny, nx + nw + 16, ny + nh];
        if (!free(nb)) { ny = core[1] - cr - nh - 5; nb = [nx, ny, nx + nw + 16, ny + nh]; }          // no room under the worker: over it
        doing = free(nb) ? doing : null;          // nor there: the line over the picture says it
      }
      if (doing) {
        ctx.fillStyle = 'rgba(10, 13, 21, .8)'; ctx.fillRect(nx, ny, nw + 16, nh);
        ctx.textAlign = 'center'; ctx.font = '600 ' + f1 + 'px ' + SANS; ctx.fillStyle = '#fff'; ctx.fillText(line, nx + 8 + nw / 2, ny + 4 + f1 / 2);
        if (f2) { ctx.font = f2 + 'px ' + MONO; ctx.fillStyle = stand.id === 'live' ? '#6ee7b7' : stand.id === 'quiet' ? '#ffc266' : '#a7aebf'; ctx.fillText(when, nx + 8 + nw / 2, ny + 4 + f1 + 5 + f2 / 2); }
        ctx.textAlign = 'left'; taken.push(nb); written.push(nb.concat(['core']));
      }
      var named = m.groups.filter(function (g) { return !opt.mini || !g.far; }).sort(function (a, b) { return (a.far ? 1 : 0) - (b.far ? 1 : 0) || b.n - a.n; }).slice(0, opt.mini ? 3 : 99);
      named.forEach(function (g) {
        var c = S(mem.gs[g.id]), r = L.r[g.id] * cam.s * (g.far ? 0.35 : 1), dim = lit && lit !== g.id ? 0.4 : 1;
        var big = opt.mini ? 10.5 : g.far ? (tight ? 11.5 : 12.5) : tight ? 12.5 : 14.5, small = opt.mini || (tight && g.far) ? 0 : tight ? 9.5 : 10.5, name = g.label.length > 30 ? g.label.slice(0, 29) + '\u2026' : g.label;
        var sub = g.far ? 'not reached \u00b7 ' + g.n + ' touched by ' + (g.by.length === 1 ? titleOf(g.by[0]).slice(0, 22) : g.by.length + ' others')
          : (g.files ? g.files + (g.files === 1 ? ' file' : ' files') + (g.changed ? ' \u00b7 ' + g.changed + ' changed' : '') : g.members.length + (g.kind === 'unit' ? ' in it' : '')) + (g.last && !opt.mini ? ' \u00b7 ' + T.ago(clockNow() - g.last) + ' ago' : '');
        ctx.font = '600 ' + big + 'px ' + SANS; var tw = ctx.measureText(name).width;
        if (small) { ctx.font = small + 'px ' + MONO; tw = Math.max(tw, ctx.measureText(sub).width); }
        var h = big + (small ? small + 5 : 0), turn = Math.atan2(c[1] - core[1], c[0] - core[0]);
        var at = place(c, r + 5, tw + 8, h + 6, turn, [0, 0.7, -0.7, 1.4, -1.4, 2.1, -2.1, Math.PI], g.far || opt.mini ? [0, 14] : [0, 20, 44, 72]);
        if (!at) return;
        taken.push(at.b); written.push(at.b.concat([g.head || g.id])); hits.push(at.b.concat([g.id]));
        if (at.led && !g.far) lead(c, at.b, 'rgba(160, 178, 235, ' + 0.45 * dim + ')');
        ctx.fillStyle = 'rgba(13, 17, 27, ' + (g.far ? 0.4 : 0.62) + ')'; ctx.fillRect(at.x, at.y, tw + 8, h + 6);
        ctx.font = '600 ' + big + 'px ' + SANS; ctx.fillStyle = g.far ? 'rgba(198, 206, 228, ' + 0.92 * dim + ')' : 'rgba(242, 243, 246, ' + 0.97 * dim + ')'; ctx.fillText(name, at.x + 4, at.y + 3 + big / 2);
        if (small) { ctx.font = small + 'px ' + MONO; ctx.fillStyle = 'rgba(167, 176, 202, ' + (g.far ? 0.88 : 0.96) * dim + ')'; ctx.fillText(sub, at.x + 4, at.y + 3 + big + 4 + small / 2); }
        if (lit === g.id || overG === g.id) { ctx.strokeStyle = 'rgba(160, 178, 235, .8)'; ctx.lineWidth = 1; ctx.strokeRect(at.x + 0.5, at.y + 0.5, tw + 7, h + 5); }
      });
      var most = opt.mini ? 3 : 40, put = 0, fs = opt.mini ? 9.5 : 11;
      ctx.font = fs + 'px ' + MONO;
      labels.sort(function (a, b) { return a.rank - b.rank; }).forEach(function (l) {
        if (put >= most && l.rank >= 1) return;
        var newest = l.n.id === order[0], text = (newest ? '\u25b8 ' : '') + (l.n.label.length > 26 ? l.n.label.slice(0, 25) + '\u2026' : l.n.label), tw = ctx.measureText(text).width + 10, h = fs + 7;
        var gc = S(mem.gs[l.n.g]), ox = l.p[0] - gc[0], oy = l.p[1] - gc[1], turn = Math.abs(ox) + Math.abs(oy) > 1 ? Math.atan2(oy, ox) : Math.atan2(l.p[1] - core[1], l.p[0] - core[0]);
        var at = place(l.p, l.r + 4, tw, h, turn, [0, 0.8, -0.8, 1.6, -1.6, 2.4, -2.4, Math.PI], opt.mini ? [0, 10] : [0, 14, 30, 52]);
        if (!at && !l.rank) { var fx = Math.max(2, Math.min(W - tw - 2, l.p[0] + l.r + 6)), fy = Math.max(2, Math.min(H - h - 2, l.p[1] - h / 2)); at = { x: fx, y: fy, b: [fx, fy, fx + tw, fy + h], led: false, forced: true }; }      // the one under the pointer is always named
        if (!at) return;
        put++; taken.push(at.b); if (!at.forced) written.push(at.b.concat([l.n.id]));
        var dim = lit && lit !== l.n.g ? 0.35 : 1;
        ctx.globalAlpha = dim; if (at.led) lead(l.p, at.b, rgba(l.col, 0.6));
        ctx.fillStyle = 'rgba(11, 15, 24, .9)'; ctx.fillRect(at.x, at.y, tw, h);
        ctx.strokeStyle = l.rank === 0 ? '#fff' : newest ? '#ffb38f' : rgba(l.col, 0.9); ctx.lineWidth = newest ? 1.8 : 1; ctx.strokeRect(at.x + 0.5, at.y + 0.5, tw - 1, h - 1);
        ctx.fillStyle = '#f2f3f6'; ctx.fillText(text, at.x + 5, at.y + h / 2 + 0.5); ctx.globalAlpha = 1;
      });
      if (!m.nodes.length) {
        ctx.textAlign = 'center'; ctx.font = (opt.mini ? 11 : 13) + 'px ' + MONO; ctx.fillStyle = 'rgba(167,174,191,.9)';
        ctx.fillText(e ? 'nothing touched yet' : 'no calls of its own to show', core[0], Math.min(H - 12, core[1] + cr + (opt.mini ? 18 : 30)));
      }
      drawn = ts;
    }
    function tick(ts) {
      if (dead) return;
      raf = requestAnimationFrame(tick);
      if (!cv.isConnected) { if (ts - last > 4000) stop(); return; }
      if (!seen || document.hidden || !m || !W) { last = ts; return; }
      var lively = ts < busyUntil || hover || (w && w.state === 'working' && e && clockNow() - e.last < 200);
      if (ts - drawn < (lively ? 32 : 240)) return;
      var dt = Math.min(0.25, (ts - last) / 1000 || 0); last = ts;
      if (step(dt)) busyUntil = ts + 600;
      render(ts);
    }
    function stop() { dead = true; cancelAnimationFrame(raf); if (io) io.disconnect(); if (ro) ro.disconnect(); }
    function under(ev) { var r = cv.getBoundingClientRect(); return T.pick(pts, ev.clientX - r.left, ev.clientY - r.top, 9); }
    cv.addEventListener('mousemove', function (ev) {
      var id = under(ev), r = cv.getBoundingClientRect(), gid = id || opt.mini ? '' : groupAt(ev.clientX - r.left, ev.clientY - r.top); if (id === 'core') id = '';
      if (id !== hover || gid !== overG) { hover = id; overG = gid; kick(); }
      cv.style.cursor = id || gid || opt.mini ? 'pointer' : 'default';
      if (opt.onhover) opt.onhover(id ? m.byId[id] : null, ev.clientX - r.left, ev.clientY - r.top, m, pts.core);
    });
    cv.addEventListener('mouseleave', function () { if (hover || overG) { hover = ''; overG = ''; kick(); } if (opt.onhover) opt.onhover(null); });
    function groupAt(x, y) { var h = hits.filter(function (b) { return x >= b[0] && x <= b[2] && y >= b[1] && y <= b[3]; })[0]; return h ? h[4] : ''; }
    if (!opt.mini) cv.addEventListener('click', function (ev) {
      var id = under(ev), r = cv.getBoundingClientRect(), gid = id ? '' : groupAt(ev.clientX - r.left, ev.clientY - r.top); if (id === 'core') id = '';
      if (gid && opt.ongroup) { opt.ongroup(m.groups.filter(function (g) { return g.id === gid; })[0]); return; }
      choose(id); if (opt.onpick) opt.onpick(id ? m.byId[id] : null, true);
    });
    function choose(id) { chosen = id && m && m.byId[id] ? id : ''; kick(); }
    function light(gid) { lit = lit === gid ? '' : gid; kick(); return lit; }
    var io = 'IntersectionObserver' in window ? new IntersectionObserver(function (es) { seen = es[0].isIntersecting; }) : null; if (io) io.observe(cv);
    var ro = 'ResizeObserver' in window ? new ResizeObserver(size) : null; if (ro) ro.observe(cv); else window.addEventListener('resize', size);
    size();
    if (!still) raf = requestAnimationFrame(tick);
    self.set = set; self.choose = choose; self.light = light; self.stop = stop; self.size = size;
    self.model = function () { return m; }; self.chosen = function () { return chosen; }; self.lit = function () { return lit; };
    self.paint = function () { step(1); render(performance.now()); };
    self.probe = function () { return { names: written.map(function (b) { return b.slice(); }), dots: pts, overlaps: T.overlaps(written, pts), tentacles: T.reach(m, refOf(e), 900, 18, 10).length }; };          // for whoever checks the picture
    return self;
  }

  // ---- words about a worker's graph
  function verbClass(v) { return 'tg-v tg-v-' + (VERB[v] || 'unit'); }
  function why(w, e) {
    if (e && (e.nodes || []).length) return '';
    if (e) return 'Nothing touched yet: it has made no call the board can read.';
    if (w && w.host) return 'It works on ' + w.host + ': its calls stay on that station, the board has what the floor says.';
    if (w && w.vendor && w.vendor !== 'claude') return 'It keeps no transcript the board reads.';
    return 'Nothing touched yet.';
  }
  function logLine(l, secs) {
    var li = el('li'); li.appendChild(el('time', null, l.now ? 'now' : T.clock(l.at, secs))); li.appendChild(el('i', verbClass(l.verb), l.verb)); li.appendChild(el('span', null, l.thing)); return li;
  }
  function counters(into, e) {
    into.innerHTML = ''; if (!into.classList.contains('tg-counters')) into = into.appendChild(el('div', 'tg-counters'));
    var unit = e && e.kind === 'run' ? '' : ' calls';
    ((e && e.counters) || [['read', 0], ['changed', 0], ['ran', 0], ['sent out', 0]]).forEach(function (c) { var s = el('span', 'tg-count tg-c-' + String(c[0]).replace(/\W+/g, '-')); s.appendChild(el('b', null, String(c[1]))); s.appendChild(el('i', null, c[0])); s.title = c[1] + ' ' + c[0] + unit + (unit ? ' in its last calls (a file touched five times counts five)' : ''); into.appendChild(s); });
  }

  // ---- small, in the profile: the picture, the counters, the last calls. A click opens it full size.
  function dock(p, w) {
    p.innerHTML = '';
    var b = el('button', 'tg-dock'); b.type = 'button'; b.title = 'Open the graph full size: what it is doing and everything it touches';
    var cv = el('canvas', 'tg-mini'); cv.setAttribute('aria-hidden', 'true'); b.appendChild(cv);
    var cap = el('span', 'tg-dock-cap'), capText = el('b', null, 'What it touches'); cap.appendChild(capText); cap.appendChild(el('i', 'tg-dock-open', 'Open full size')); b.appendChild(cap);
    var cs = el('div', 'tg-counters'), log = el('ol', 'tg-log'), none = el('p', 'tg-why');
    p.appendChild(b); p.appendChild(cs); p.appendChild(log); p.appendChild(none);
    var st = p._touch = { view: View(cv, { mini: true }), cs: cs, log: log, none: none, b: b, cap: capText, sig: '', key: '' };
    b.addEventListener('click', function () { open(st.key); });
    hook(); fresh(p, w);
  }
  function fresh(p, w) {
    var st = p._touch; if (!st) return;
    var k = T.keyOf(window.TOUCH, w), e = by()[k] || null, sig = JSON.stringify([k, T.sig(e), w.state]);
    st.key = k;
    var stand = T.state(w, e, clockNow()); st.cap.textContent = 'What it touches' + (e && e.last && stand.id !== 'live' ? ' · ' + stand.words : e && e.last ? ' · live' : '');
    if (st.sig === sig) return; st.sig = sig;
    st.view.set(k, e, w); counters(st.cs, e);
    st.log.innerHTML = ''; ((e && e.log) || []).slice(-4).reverse().forEach(function (l) { st.log.appendChild(logLine(l)); });
    st.none.textContent = why(w, e); st.none.hidden = !st.none.textContent;
    st.b.setAttribute('aria-label', 'What ' + titleOf(k) + ' touches: ' + ((e && e.counters) || []).map(function (c) { return c[1] + ' ' + c[0]; }).join(', ') + '. Open the graph full size.');
  }

  // ---- full size, over the house and the profile
  var full = null, hooked = false, clock = 0;
  function hook() { if (hooked || !C || !C.live || (window.OPS == null)) return; hooked = true; C.live(function () { if (full) show(full.key, true); }); }
  function panel(name, title) { var s = el('section', 'tg-panel tg-p-' + name), h = el('header'); h.appendChild(el('b', null, title)); var tag = el('span', 'tg-tag'); h.appendChild(tag); s.appendChild(h); var body = el('div', 'tg-body'); s.appendChild(body); return { box: s, tag: tag, body: body }; }
  function open(key) {
    if (full) { show(key); return; }
    var host = document.getElementById('deck') || document.getElementById('househome') || document.body;
    var box = el('div', 'tg-full'); box.setAttribute('role', 'dialog'); box.setAttribute('aria-label', 'What a worker is doing and what it touches'); box.tabIndex = -1;
    var bar = el('header', 'tg-bar'), who = el('div', 'tg-who'), stats = el('div', 'tg-stats'), x = el('button', 'tg-close', 'Close'); x.type = 'button'; x.title = 'Back to the house (Esc)';
    bar.appendChild(who); bar.appendChild(stats); bar.appendChild(x);
    var stage = el('div', 'tg-stage'), cv = el('canvas', 'tg-canvas'), tip = el('div', 'tg-tip'), key2 = el('div', 'tg-key'), sub = el('p', 'tg-doing'); tip.hidden = true;
    cv.setAttribute('role', 'img'); stage.appendChild(cv); stage.appendChild(sub); stage.appendChild(key2); stage.appendChild(tip);
    var side = el('div', 'tg-side'), under = el('div', 'tg-under');
    var P = { log: panel('log', 'Crawl log'), sel: panel('sel', 'Selected'), count: panel('count', 'Counters'), groups: panel('groups', 'Clusters'), heat: panel('heat', 'Heat'), radar: panel('radar', 'Where the work went') };
    side.appendChild(P.sel.box); side.appendChild(P.log.box);
    under.appendChild(P.count.box); under.appendChild(P.groups.box); under.appendChild(P.heat.box); under.appendChild(P.radar.box);
    box.appendChild(bar); box.appendChild(stage); box.appendChild(side); box.appendChild(under);
    var nav = document.querySelector('header.top'), pinned = nav && /sticky|fixed/.test(getComputedStyle(nav).position) ? nav.offsetHeight : 0;
    box.style.setProperty('--tg-top', pinned + 'px'); box.style.scrollMarginTop = (pinned + 10) + 'px';
    host.classList.add('tg-host'); host.appendChild(box); document.body.classList.add('tg-open');
    full = { box: box, host: host, who: who, stats: stats, sub: sub, tip: tip, P: P, key: '', sig: '', back: document.activeElement, legend: key2 };
    full.view = View(cv, { onhover: hoverTip, onpick: function (n, clicked) { if (n && clicked) act(n); else select(n); }, ongroup: pickGroup });
    x.addEventListener('click', close);
    window.addEventListener('keydown', onKey, true);
    clock = setInterval(function () { if (full) ages(); }, 1000);
    hook(); show(key); x.focus();
    if (box.getBoundingClientRect().top < pinned || box.getBoundingClientRect().top > window.innerHeight * 0.4) box.scrollIntoView({ block: 'start', behavior: still ? 'auto' : 'smooth' });
  }
  function onKey(ev) { if (ev.key === 'Escape' && full) { ev.stopImmediatePropagation(); ev.preventDefault(); close(); } }
  function close() {
    if (!full) return;
    var f = full; full = null; clearInterval(clock); window.removeEventListener('keydown', onKey, true);
    f.view.stop(); f.box.remove(); f.host.classList.remove('tg-host'); document.body.classList.remove('tg-open');
    if (f.back && f.back.isConnected && f.back.focus) f.back.focus();
  }
  function hoverTip(n, x, y, m, core) {
    var tip = full && full.tip; if (!tip) return;
    if (!n) { tip.hidden = true; return; }
    tip.innerHTML = '';
    tip.appendChild(el('b', null, n.label)); tip.appendChild(el('i', 'tg-v-' + (n.kind in COL ? n.kind : 'tool'), T.WORD[n.kind] || n.kind));
    var p = T.path(m, n); if (p && p !== n.label) tip.appendChild(el('span', 'tg-tip-path', p));
    tip.appendChild(el('span', null, (n.m > 1 ? n.m + ' older things folded together · ' : '') + 'touched ' + n.n + (n.n === 1 ? ' time' : ' times') + (n.last ? ' · last ' + T.ago(clockNow() - n.last) + ' ago' : '')));
    if (n.also) tip.appendChild(el('span', 'tg-tip-also', 'also touched by ' + n.also.map(titleOf).join(', ')));
    var a = T.action(n); tip.appendChild(el('span', 'tg-tip-do', a.go === 'worker' ? 'Click: open this agent' : a.go === 'page' ? 'Click: go to ' + (n.kind === 'run' ? 'the run' : 'its branch') : 'Click: every time it was touched'));
    tip.hidden = false;
    var w = tip.offsetWidth, h = tip.offsetHeight, st = tip.parentNode.getBoundingClientRect();
    var cx = core ? core[0] : st.width / 2, cy = core ? core[1] : st.height / 2;          // on the side away from the worker
    tip.style.left = Math.max(6, Math.min(st.width - w - 6, x < cx ? x - w - 14 : x + 14)) + 'px'; tip.style.top = Math.max(6, Math.min(st.height - h - 6, y < cy ? y - h - 12 : y + 16)) + 'px';
  }
  // a click on a node (touch.js action): open an agent, go to a page, or show the thing
  function act(n) {
    var a = T.action(n);
    if (a.go === 'worker') { if (window.HouseView && window.HouseView.pin) window.HouseView.pin(a.key); if (by()[a.key] || workerOf(a.key)) { show(a.key); return; } }
    if (a.go === 'page') { location.href = ROOT + a.href; return; }
    select(n);
  }
  function button(text, title, go) { var b = el('button', 'tg-link', text); b.type = 'button'; if (title) b.title = title; b.addEventListener('click', go); return b; }
  function select(n) {
    if (!full) return;
    var P = full.P.sel, e = by()[full.key] || null, m = full.view.model(), w = workerOf(full.key); P.body.innerHTML = '';
    full.view.choose(n ? n.id : '');
    full.P.log.body.querySelectorAll('li').forEach(function (li) { li.classList.toggle('on', !!n && li.dataset.id === n.id); });
    if (!n) {          // nothing picked: the worker itself
      P.tag.textContent = e && e.kind === 'run' ? 'the relay' : (w && w.kind) || (e && e.kind) || '';
      P.body.appendChild(el('h4', null, titleOf(full.key)));
      var what = (w && C && (C.doing(w) || w.what)) || (e && e.title) || ''; if (what) P.body.appendChild(el('p', 'tg-sel-what', what));
      var facts = el('p', 'tg-sel-facts');
      if (e && e.calls) facts.appendChild(el('span', null, e.kind === 'run' ? e.calls + (e.calls === 1 ? ' leg' : ' legs') + (e.first ? ' since ' + T.clock(e.first) : ' on record') : 'its last ' + e.calls + ' calls, since ' + T.clock(e.first) + (e.more ? ' (older ones are not shown)' : '')));
      if (e && e.cut) facts.appendChild(el('span', null, e.cut + ' older things are left out to keep the page light'));
      if (w && w.model) facts.appendChild(el('span', null, w.model)); if (e && e.gone) facts.appendChild(el('span', null, 'it is over: this is what it left'));
      if (e && e.note) facts.appendChild(el('span', null, e.note));
      P.body.appendChild(facts);
      var msg = why(w, e); if (msg) P.body.appendChild(el('p', 'tg-why', msg));
      var parent = (e && e.parent) || (w && w.parent);
      if (parent && (by()[parent] || workerOf(parent))) P.body.appendChild(button('← sent by ' + titleOf(parent), 'Open the one that sent it', function () { act({ kind: 'subagent', to: parent }); }));
      var kids = m ? m.nodes.filter(function (x) { return x.kind === 'subagent' && x.to; }) : [];
      if (kids.length) { P.body.appendChild(el('h5', null, 'Agents it sent')); kids.slice(-8).reverse().forEach(function (k) { P.body.appendChild(button(k.label + ' →', 'Open this agent', function () { act(k); })); }); }
      P.body.appendChild(el('p', 'tg-hint', 'Click a dot: a file says every time it was touched, an agent opens, a branch goes to its room.'));
      return;
    }
    P.tag.textContent = T.WORD[n.kind] || n.kind;
    P.body.appendChild(el('h4', null, n.label));
    var path = T.path(m, n); P.body.appendChild(el('p', 'tg-sel-path', path));
    if (n.m > 1) P.body.appendChild(el('p', 'tg-sel-what', n.m + ' older things, folded together to keep the picture readable.'));
    if (n.also) { var al = el('p', 'tg-sel-also', 'Also touched by '); n.also.forEach(function (k) { al.appendChild(button(titleOf(k), 'Open its graph', function () { act({ kind: 'subagent', to: k }); })); }); P.body.appendChild(al); }
    var ts = T.touches(e, n), ol = el('ol', 'tg-log tg-touches');
    P.body.appendChild(el('h5', null, 'Touched ' + n.n + (n.n === 1 ? ' time' : ' times') + (ts.length < n.n ? ' · the last ' + ts.length : '')));
    ts.forEach(function (t) { ol.appendChild(logLine({ at: t.at, verb: t.verb, thing: T.ago(clockNow() - t.at) + ' ago' }, true)); });
    P.body.appendChild(ol);
    var a = T.action(n);
    if (a.go === 'worker') P.body.appendChild(button('Open this agent →', '', function () { act(n); }));
    if (a.go === 'page') { var l = el('a', 'tg-link', (n.kind === 'run' ? 'The run' : 'Its branch') + ' →'); l.href = ROOT + a.href; P.body.appendChild(l); }
    P.body.appendChild(button('Back to ' + titleOf(full.key), '', function () { select(null); }));
  }
  function pickGroup(g) {
    if (!full || !g) return;
    var on = full.view.light(g.id), P = full.P.sel, m = full.view.model();
    full.P.groups.body.querySelectorAll('.tg-group').forEach(function (b) { b.classList.toggle('on', !!on && b.dataset.g === g.id); });
    if (!on) { select(null); return; }
    full.view.choose(''); P.body.innerHTML = ''; P.tag.textContent = g.far ? 'not reached' : 'cluster';
    P.body.appendChild(el('h4', null, g.label));
    if (g.far) {
      P.body.appendChild(el('p', 'tg-sel-what', titleOf(full.key) + ' did not touch anything here. ' + g.n + (g.n === 1 ? ' thing was' : ' things were') + ' touched by:'));
      g.by.forEach(function (k) { P.body.appendChild(button(titleOf(k) + ' →', 'Open its graph', function () { act({ kind: 'subagent', to: k }); })); });
    } else {
      P.body.appendChild(el('p', 'tg-sel-what', (g.files ? g.files + (g.files === 1 ? ' file' : ' files') + (g.changed ? ', ' + g.changed + ' changed' : '') : g.members.length + ' things') + ', the newest first:'));
      var ol = el('ol', 'tg-log');
      g.members.map(function (id) { return m.byId[id]; }).sort(function (a, b) { return b.last - a.last; }).slice(0, 30).forEach(function (n) {
        var li = logLine({ at: n.last, verb: n.t && n.t.length ? n.t[n.t.length - 1][1] : T.WORD[n.kind] || n.kind, thing: n.label + (n.n > 1 ? ' ×' + n.n : '') }); li.tabIndex = 0; li.classList.add('tg-pickable');
        var go = function () { select(n); }; li.addEventListener('click', go); li.addEventListener('keydown', function (ev) { if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); go(); } }); ol.appendChild(li); });
      P.body.appendChild(ol);
      var head = g.head && m.byId[g.head], a = head && T.action(head); if (a && a.go === 'page') { var l = el('a', 'tg-link', 'Its branch →'); l.href = ROOT + a.href; P.body.appendChild(l); }
    }
    P.body.appendChild(button('Back to ' + titleOf(full.key), '', function () { pickGroup(g); }));
  }
  function stat(into, label, value, cls) { var s = el('span', 'tg-stat' + (cls ? ' ' + cls : '')); s.appendChild(el('i', null, label)); s.appendChild(el('b', null, String(value))); into.appendChild(s); return s; }
  function ages() {
    var e = by()[full.key] || null, a = full.stats.querySelector('.tg-age b'), w = workerOf(full.key);
    if (a && e && e.last) a.textContent = T.ago(clockNow() - e.last) + ' ago';
    var d = T.doing(e, clockNow()), st = T.state(w, e, clockNow()), sub = full.sub; sub.innerHTML = '';
    if (d) { sub.appendChild(el('b', null, d.text)); sub.appendChild(el('i', 'tg-is-' + st.id, T.since(d, st))); }
    if (full.task) sub.appendChild(el('span', null, full.task));
    var live = full.stats.querySelector('.tg-live'); if (live) { var stand = T.state(w, e, clockNow()); live.className = 'tg-stat tg-live tg-is-' + stand.id; live.lastChild.textContent = stand.words; }
  }
  function show(key, again) {
    if (!full) return;
    var e = by()[key] || null, w = workerOf(key), sig = JSON.stringify([key, T.sig(e), w && w.state, w && w.doing]);
    if (again && full.sig === sig) { ages(); return; }
    var swap = full.key !== key; full.key = key; full.sig = sig;
    full.view.set(key, e, w);
    var m = full.view.model(), run = e && e.kind === 'run';
    // the bar: who, and the numbers
    full.who.innerHTML = ''; full.who.appendChild(el('i', 'tg-kind', run ? 'relay' : (w && w.kind === 'session') || (e && e.kind === 'session') ? 'session' : 'agent')); full.who.appendChild(el('b', null, titleOf(key)));
    full.stats.innerHTML = '';
    var reach = T.reach(m, refOf(e), 900, 18, 10).length;
    var live = el('span', 'tg-stat tg-live'); live.appendChild(el('u')); live.appendChild(document.createTextNode('')); full.stats.appendChild(live);
    stat(full.stats, run ? 'last record' : 'last call', e && e.last ? T.ago(clockNow() - e.last) : '–', 'tg-age');
    stat(full.stats, run ? 'legs' : 'calls', e ? e.calls || 0 : 0); stat(full.stats, 'tentacles', reach); stat(full.stats, 'things', m.nodes.length); stat(full.stats, 'files', e ? e.files || 0 : 0);
    stat(full.stats, 'clusters', m.groups.filter(function (g) { return !g.far; }).length + '/' + m.groups.length);
    var shared = m.nodes.filter(function (n) { return n.also; }).length; if (shared) stat(full.stats, 'shared', shared, 'tg-shared').title = 'Things another worker on the floor touched too: the dots with a dashed yellow ring';
    full.task = (w && (w.what || (C && C.doing(w)))) || (e && e.title) || '';
    full.box.setAttribute('aria-label', 'What ' + titleOf(key) + ' is doing and what it touches');
    // the key: what the marks mean, only the kinds this graph has
    full.legend.innerHTML = ''; full.legend.appendChild(el('b', null, 'things on the map')); var kinds = {}; m.nodes.forEach(function (n) { if (!n.head) kinds[n.kind] = (kinds[n.kind] || 0) + 1; });
    Object.keys(kinds).sort(function (a, b) { return kinds[b] - kinds[a]; }).slice(0, 8).forEach(function (k) { var s = el('span', 'tg-key-' + k); s.appendChild(el('u')); s.appendChild(document.createTextNode((T.WORD[k] || k) + ' ' + kinds[k])); var u = s.querySelector('u'); if (k === 'named' || k === 'reported' || k === 'web') { u.style.border = '1.5px solid ' + (COL[k] || '#a7aebf'); u.style.borderRadius = '50%'; } else if (k === 'folder') { u.style.border = '1.5px dotted ' + COL[k]; } else u.style.background = COL[k] || '#a7aebf'; full.legend.appendChild(s); });
    // the log, newest first: a click on a line picks its thing
    var P = full.P; P.log.body.innerHTML = ''; P.log.tag.textContent = e && !run ? T.rate(e) + ' calls/min, last 10 min' : '';
    var ol = el('ol', 'tg-log');
    ((e && e.log) || []).slice().reverse().forEach(function (l) { var li = logLine(l, true); li.dataset.id = l.id || ''; if (l.id && m.byId[l.id]) { li.tabIndex = 0; li.classList.add('tg-pickable'); var go = function () { select(m.byId[l.id]); }; li.addEventListener('click', go); li.addEventListener('keydown', function (ev) { if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); go(); } }); } ol.appendChild(li); });
    if (!ol.firstChild) P.log.body.appendChild(el('p', 'tg-why', why(w, e) || 'No calls yet.')); else P.log.body.appendChild(ol);
    counters(P.count.body, e); P.count.tag.textContent = e && e.calls ? (run ? 'this run' : 'calls, of its last ' + e.calls) : '';
    // the clusters: a click lights one up
    P.groups.body.innerHTML = ''; P.groups.tag.textContent = String(m.groups.filter(function (g) { return !g.far; }).length);
    var total = Math.max(1, m.groups.reduce(function (a, g) { return Math.max(a, g.n); }, 0));
    m.groups.slice().sort(function (a, b) { return (a.far ? 1 : 0) - (b.far ? 1 : 0) || b.n - a.n; }).forEach(function (g) {
      var row = el('button', 'tg-group' + (g.far ? ' tg-far' : '')); row.type = 'button'; row.title = g.far ? 'Not reached by this one: touched by ' + g.by.map(titleOf).join(', ') : 'Light up this cluster';
      row.appendChild(el('span', null, g.label)); row.appendChild(el('b', null, g.far ? 'not reached' : g.changed ? g.changed + ' changed · ' + g.n : String(g.n))); row.dataset.g = g.id;
      var bar = el('i'); bar.style.width = Math.round(100 * g.n / total) + '%'; bar.style.background = g.far ? 'rgba(160,178,235,.25)' : COL[g.kind] || '#8ea2c8'; row.appendChild(bar);
      row.addEventListener('click', function () { pickGroup(g); });
      if (full.view.lit() === g.id) row.classList.add('on');
      P.groups.body.appendChild(row);
    });
    if (!m.groups.length) P.groups.body.appendChild(el('p', 'tg-why', 'No clusters yet.'));
    // the heat of the hour before its last call, a row per kind of work
    P.heat.body.innerHTML = ''; P.heat.box.hidden = P.radar.box.hidden = !e || !e.heat;
    if (e && e.heat) {
      P.heat.tag.textContent = 'the last hour, to now'; var heat = T.hour(e.heat, e.last, clockNow());
      var names = ['read', 'changed', 'ran', 'searched', 'sent'], cols = [COL.read, COL.changed, COL.command, COL.search, COL.subagent], top = 1; heat.forEach(function (r) { r.forEach(function (v) { top = Math.max(top, v); }); });
      heat.forEach(function (r, i) { var row = el('div', 'tg-heat-row'); row.appendChild(el('span', null, names[i])); var cells = el('div');
        r.forEach(function (v, k) { var c = el('i'); if (v) { c.style.background = cols[i]; c.style.opacity = String(0.3 + 0.7 * v / top); c.title = v + ' ' + names[i] + ', ' + (r.length - k) * 2 + ' min ago'; } cells.appendChild(c); });
        row.appendChild(cells); P.heat.body.appendChild(row); });
      // the kinds of work as a shape: six spokes, each as long as its share of the busiest
      var axes = m.groups.filter(function (g) { return !g.far; }).sort(function (a, b) { return b.n - a.n; }).slice(0, 6).map(function (g) { return [g.label.length > 13 ? g.label.slice(0, 12) + '…' : g.label, g.n]; });
      P.radar.box.hidden = axes.length < 3; while (axes.length < 3) axes.push(['', 0]);
      var mx = Math.max.apply(null, axes.map(function (a) { return a[1] || 0; }).concat([1]));
      var svg = '<svg viewBox="-104 -62 208 124" aria-hidden="true">', ring = function (k) { return axes.map(function (a, i) { var t = -Math.PI / 2 + i * TAU / axes.length; return (Math.cos(t) * 42 * k).toFixed(1) + ',' + (Math.sin(t) * 42 * k).toFixed(1); }).join(' '); };
      [1, 0.66, 0.33].forEach(function (k) { svg += '<polygon points="' + ring(k) + '" fill="none" stroke="rgba(160,178,235,.2)"/>'; });
      svg += '<polygon points="' + axes.map(function (a, i) { var t = -Math.PI / 2 + i * TAU / axes.length, k = 0.08 + 0.92 * Math.sqrt((a[1] || 0) / mx); return (Math.cos(t) * 42 * k).toFixed(1) + ',' + (Math.sin(t) * 42 * k).toFixed(1); }).join(' ') + '" fill="rgba(91,157,255,.25)" stroke="#8fd3ff" stroke-width="1.5"/>';
      axes.forEach(function (a, i) { var t = -Math.PI / 2 + i * TAU / axes.length; if (!a[0]) return; var cx = Math.cos(t); svg += '<text x="' + (cx * 48).toFixed(1) + '" y="' + (Math.sin(t) * 52 + 3).toFixed(1) + '" text-anchor="' + (cx > 0.3 ? 'start' : cx < -0.3 ? 'end' : 'middle') + '">' + a[0] + ' ' + (a[1] || 0) + '</text>'; });
      P.radar.body.innerHTML = svg + '</svg>'; P.radar.tag.textContent = 'things per cluster';
    }
    if (swap) select(null); else { var ch = full.view.chosen(); select(ch && m.byId[ch] ? m.byId[ch] : null); }
    ages();
  }
  function key(w) { return T.has(w, T.entry(window.TOUCH, w)) ? T.keyOf(window.TOUCH, w) : ''; }
  window.TouchDraw = { key: key, dock: dock, fresh: fresh, open: open, close: close, isOpen: function () { return !!full; }, probe: function () { return full ? full.view.probe() : null; } };
  // ?touch (with ?pin=<who>): the picture opens full size on the frog the page was asked to pick
  var q = new URLSearchParams(location.search);
  if (q.has('touch')) window.addEventListener('load', function () { setTimeout(function () { var hv = window.HouseView, f = hv && hv.shown && hv.shown(); if (f && key(f.w)) open(key(f.w)); }, 300); });
})();
