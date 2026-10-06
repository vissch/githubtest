// The graphs page (graphs.html): the work in numbers, drawn from data/graphs.js (src_graphs.py), which ops.py makes
// again on every read; the page reads it again every 20 seconds (crew.js), so nobody redraws a graph by hand.
// Every mark says its number on hover and every graph has its table underneath. A mark that stands for a place on the
// board goes there on a click: a room to the house, a branch to its room, a status to its models.
// On the control screen (index.html) the same graphs stand under the house, and a mark whose place is on that page
// (a room of the house, a branch's room, the models of a status) takes the page there instead of leaving it.
// The scales are plain functions (test_assetboard.py runs them under node).
(function (root) {
  'use strict';
  // clean axis steps: the smallest of 1, 2, 5 x 10^n that gives at most `most` steps up to max
  function ticks(max, most) {
    if (!(max > 0)) return [0, 1];
    var raw = max / (most || 4), p = Math.pow(10, Math.floor(Math.log10(raw))), step = [1, 2, 5, 10].map(function (m) { return m * p; }).filter(function (s) { return s >= raw; })[0];
    step = Math.max(1, step);
    var out = []; for (var v = 0; v < max + step; v += step) out.push(v);
    return out;
  }
  function fmt(n) { return n >= 10000 ? (n / 1000).toFixed(n >= 100000 ? 0 : 1) + 'K' : String(Math.round(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ','); }
  function hourLabel(h) { return h.slice(11) + ':00'; }
  function dayLabel(d) { var t = new Date(d.slice(0, 10) + 'T12:00:00'); return ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][t.getDay()] + ' ' + t.getDate(); }
  var pure = { ticks: ticks, fmt: fmt, hourLabel: hourLabel, dayLabel: dayLabel };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }

  var C = window.Crew, B = window.Board, page = document.getElementById('graphs');
  if (!C || !page) return;
  var el = C.el, NS = 'http://www.w3.org/2000/svg';
  var ROOM = { work: 'Workroom', lab: 'Lab', shop: 'Workshop', studio: 'Studio', plan: 'War room' };
  var ROOM_DOES = { work: 'code, docs, commits', lab: 'reading, searching, tests', shop: 'builds, Unity, tools', studio: 'films, art, animation', plan: 'plans, briefing agents' };
  var STATUS = [['IDEA', 'Idea', '#184f95'], ['NEEDS_VISUAL', 'Needs a visual', '#256abf'], ['IN_PROGRESS', 'In progress', '#3987e5'], ['READY_UNUSED', 'Ready, not used', '#6da7ec'], ['FINAL', 'Final, in the battle', '#b7d3f6']];
  var KIND = { character: 'Characters', vehicle: 'Vehicles', building: 'Buildings' };
  function css(name) { return getComputedStyle(page).getPropertyValue(name).trim(); }
  var calm = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  // to a place on the board: on this page when it is here (page.html#id with that id in this page), else to its page
  function go(href) {
    var id = href.split('#')[1], at = id && document.getElementById(id);
    if (!at) { location.href = href; return; }
    at.scrollIntoView({ behavior: calm ? 'auto' : 'smooth' });
    try { history.replaceState(null, '', '#' + id); } catch (e) { /* a page opened as a file may not: the scroll is what matters */ }
  }
  function s(tag, at, kids) { var e = document.createElementNS(NS, tag); for (var k in at || {}) e.setAttribute(k, at[k]); (kids || []).forEach(function (c) { e.appendChild(c); }); return e; }
  function text(x, y, str, at) { var t = s('text', Object.assign({ x: x, y: y }, at || {})); t.textContent = str; return t; }
  // a column or a bar: square at the baseline, rounded at the end that carries the number
  function bar(x, y, w, h, r, way) {
    r = Math.max(0, Math.min(r, w / 2, h / 2));
    if (way === 'right') return 'M' + x + ',' + y + 'h' + (w - r) + 'a' + r + ',' + r + ' 0 0 1 ' + r + ',' + r + 'v' + (h - 2 * r) + 'a' + r + ',' + r + ' 0 0 1 -' + r + ',' + r + 'h-' + (w - r) + 'z';
    return 'M' + x + ',' + (y + h) + 'v-' + (h - r) + 'a' + r + ',' + r + ' 0 0 1 ' + r + ',-' + r + 'h' + (w - 2 * r) + 'a' + r + ',' + r + ' 0 0 1 ' + r + ',' + r + 'v' + (h - r) + 'z';
  }
  // a card: its title, what it shows, a plot with a tooltip, and its table
  function card(id) {
    var c = document.getElementById(id), plot = c.querySelector('.g-plot'), tip = c.querySelector('.g-tip');
    if (!tip) { tip = el('div', 'g-tip'); tip.hidden = true; plot.appendChild(tip); }
    return { c: c, plot: plot, tip: tip, legend: c.querySelector('.g-legend'), table: c.querySelector('.g-scroll') };
  }
  function fresh(k, sig) { if (k.c.dataset.sig === sig) return false; k.c.dataset.sig = sig; Array.prototype.slice.call(k.plot.children).forEach(function (n) { if (n !== k.tip) n.remove(); }); k.tip.hidden = true; return true; }
  function tipAt(k, e, html) {
    k.tip.innerHTML = html; k.tip.hidden = false;
    var r = k.plot.getBoundingClientRect(), x = e.clientX - r.left + 14, y = e.clientY - r.top + 14;
    k.tip.style.left = Math.max(0, Math.min(r.width - k.tip.offsetWidth, x)) + 'px'; k.tip.style.top = Math.max(0, Math.min(r.height - k.tip.offsetHeight, y)) + 'px';
  }
  function row(color, label, value) { return '<span><span>' + (color ? '<i style="background:' + color + '"></i>' : '') + label + '</span><b>' + value + '</b></span>'; }
  function hover(k, g, html, href) {
    g.addEventListener('mousemove', function (e) { g.classList.add('g-on'); tipAt(k, e, html()); });
    g.addEventListener('mouseleave', function () { g.classList.remove('g-on'); k.tip.hidden = true; });
    if (href) { g.classList.add('g-go'); g.setAttribute('tabindex', '0'); g.setAttribute('role', 'link');
      g.addEventListener('click', function () { go(href); }); g.addEventListener('keydown', function (e) { if (e.key === 'Enter') go(href); }); }
  }
  function table(k, head, rows) {
    if (!k.table) return;
    var t = el('table'), th = el('tr'); head.forEach(function (h) { th.appendChild(el('th', null, h)); });
    var thead = el('thead'); thead.appendChild(th); t.appendChild(thead); var tb = el('tbody');
    rows.forEach(function (r) { var tr = el('tr'); r.forEach(function (v) { tr.appendChild(el('td', null, String(v))); }); tb.appendChild(tr); });
    t.appendChild(tb); k.table.innerHTML = ''; k.table.appendChild(t);
  }
  function empty(k, words) { k.plot.insertBefore(el('p', 'g-empty', words), k.tip); }
  function grid(svg, yt, x0, x1, y) { yt.forEach(function (v) { svg.appendChild(s('line', { x1: x0, x2: x1, y1: y(v), y2: y(v), stroke: css(v ? '--g-grid' : '--g-axis'), 'stroke-width': 1 })); svg.appendChild(text(x0 - 8, y(v) + 4, fmt(v), { 'text-anchor': 'end' })); }); }

  // ---- the work by room, hour by hour: a column an hour, a colour a room
  function rooms(G) {
    var k = card('g-rooms'), hours = G.hours || [], names = G.rooms || [];
    if (!fresh(k, JSON.stringify(hours))) return;
    if (k.legend && !k.legend.children.length) names.forEach(function (r) {
      var b = el('button'); b.type = 'button'; b.title = ROOM[r] + ': ' + ROOM_DOES[r] + '. Open it in the house.'; var i = el('i'); i.style.background = css('--g-' + r); b.appendChild(i); b.appendChild(document.createTextNode(ROOM[r]));
      b.addEventListener('click', function () { var V = window.HouseView; if (V && V.open) { V.open(r); go('#house'); } else location.href = 'house.html?room=' + r; }); k.legend.appendChild(b); });
    var total = function (h) { return names.reduce(function (n, r) { return n + (h[r] || 0); }, 0); }, top = Math.max.apply(null, hours.map(total).concat([0]));
    table(k, ['Hour'].concat(names.map(function (r) { return ROOM[r]; }), ['All']), hours.filter(total).reverse().map(function (h) { return [h.h + ':00'].concat(names.map(function (r) { return h[r] || 0; }), [total(h)]); }));
    if (!top) return empty(k, 'No tool call on this station in the last ' + hours.length + ' hours.');
    var W = 1000, H = 300, L = 46, R = 10, T = 12, Bm = 40, yt = ticks(top, 4), ymax = yt[yt.length - 1], slot = (W - L - R) / hours.length, bw = Math.min(24, slot - 2);
    var y = function (v) { return T + (H - T - Bm) * (1 - v / ymax); }, svg = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img', 'aria-label': 'Tool calls an hour by room, the last ' + hours.length + ' hours' });
    grid(svg, yt, L, W - R, y);
    hours.forEach(function (h, i) {
      var x = L + i * slot + (slot - bw) / 2, base = 0, g = s('g'), live = names.filter(function (r) { return h[r] > 0; });
      live.forEach(function (r, n) {
        var y1 = y(base + h[r]), y0 = y(base), ht = Math.max(1, y0 - y1 - (n ? 2 : 0));       // a 2 px gap of the surface between two rooms
        g.appendChild(s('path', { 'class': 'g-mark', d: bar(x, y1, bw, ht, n === live.length - 1 ? 4 : 0), fill: css('--g-' + r) })); base += h[r];
      });
      g.appendChild(s('rect', { 'class': 'g-hit', x: L + i * slot, y: T, width: slot, height: H - T - Bm }));
      hover(k, g, function () { return '<b>' + dayLabel(h.h) + ', ' + hourLabel(h.h) + '</b>' + (total(h) ? names.filter(function (r) { return h[r]; }).reverse().map(function (r) { return row(css('--g-' + r), ROOM[r], fmt(h[r])); }).join('') + row('', 'All', fmt(total(h))) : '<em>nothing</em>'); });
      svg.appendChild(g);
      var hh = +h.h.slice(11);
      if (hh % 6 === 0) svg.appendChild(text(L + i * slot + slot / 2, H - Bm + 16, hh === 0 ? dayLabel(h.h) : hourLabel(h.h), { 'text-anchor': 'middle', 'class': hh === 0 ? 'g-lab' : '' }));
      if (hh === 0 && i) svg.appendChild(s('line', { x1: L + i * slot, x2: L + i * slot, y1: T, y2: H - Bm + 4, stroke: css('--g-axis'), 'stroke-width': 1 }));
    });
    k.plot.insertBefore(svg, k.tip);
  }

  // ---- which branch got the work: a bar a branch, the longest first
  function lanes(G) {
    var k = card('g-lanes'), list = (G.lanes || []).slice(0, 8), rest = (G.lanes || []).slice(8).reduce(function (n, l) { return n + l.calls; }, 0);
    if (!fresh(k, JSON.stringify(G.lanes || []))) return;
    table(k, ['Branch', 'Tool calls'], (G.lanes || []).map(function (l) { return [l.branch, l.calls]; }));
    if (!list.length) return empty(k, 'No work in a checkout of this repository in the last seven days.');
    if (rest) list = list.concat([{ branch: 'the other ' + ((G.lanes || []).length - 8), calls: rest, other: true }]);
    var W = 480, rowH = 30, L = 150, R = 56, H = list.length * rowH + 8, top = Math.max.apply(null, list.map(function (l) { return l.calls; }));
    var svg = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img', 'aria-label': 'Tool calls by branch, the last seven days' });
    list.forEach(function (l, i) {
      var yy = 4 + i * rowH, w = Math.max(2, (W - L - R) * l.calls / top), g = s('g'), name = B ? B.short(l.branch) : l.branch;
      g.appendChild(text(L - 10, yy + 18, name.length > 20 ? name.slice(0, 19) + '…' : name, { 'text-anchor': 'end', 'class': 'g-lab' }));
      g.appendChild(s('path', { 'class': 'g-mark', d: bar(L, yy + 5, w, 18, 4, 'right'), fill: css(l.other ? '--g-off' : '--g-one') }));
      g.appendChild(text(L + w + 8, yy + 18, fmt(l.calls), { 'class': 'g-val' }));
      g.appendChild(s('rect', { 'class': 'g-hit', x: 0, y: yy, width: W, height: rowH }));
      hover(k, g, function () { return '<b>' + l.branch + '</b>' + row('', 'tool calls, 7 days', fmt(l.calls)) + (l.other ? '' : '<em>click: its room on the branches page</em>'); }, l.other ? '' : 'floor.html#' + (B ? B.slug(l.branch) : ''));
      svg.appendChild(g);
    });
    k.plot.insertBefore(svg, k.tip);
  }

  // ---- a line over time (what waited on the owner): the samples ops.py keeps
  function line(id, G, key, words) {
    var k = card(id), rows = (G.history || []).filter(function (r) { return r[key] != null; });
    if (!fresh(k, JSON.stringify([rows.length, rows.length ? rows[rows.length - 1] : 0, G.since]))) return;
    table(k, ['When', words], rows.slice().reverse().slice(0, 200).map(function (r) { return [new Date(r.t * 1000).toLocaleString(), r[key]]; }));
    var since = new Date((G.since || 0) * 1000);
    if (rows.length < 2 || rows[rows.length - 1].t - rows[0].t < 1800) return empty(k, 'Collecting: this station has kept readings since ' + since.toLocaleString() + '. The line shows once there is half an hour of them.');
    var W = 480, H = 220, L = 40, R = 46, T = 14, Bm = 30, t0 = rows[0].t, t1 = rows[rows.length - 1].t, top = Math.max.apply(null, rows.map(function (r) { return r[key]; })), yt = ticks(top, 4), ymax = yt[yt.length - 1];
    var x = function (t) { return L + (W - L - R) * (t - t0) / (t1 - t0); }, y = function (v) { return T + (H - T - Bm) * (1 - v / ymax); };
    var svg = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img', 'aria-label': words + ' over time' });
    grid(svg, yt, L, W - R, y);
    var d = '', last = rows[rows.length - 1];
    rows.forEach(function (r, i) { d += (i ? 'L' : 'M') + x(r.t).toFixed(1) + ',' + y(i ? rows[i - 1][key] : r[key]).toFixed(1) + 'L' + x(r.t).toFixed(1) + ',' + y(r[key]).toFixed(1); });      // a count holds until the next reading: steps, not slopes
    svg.appendChild(s('path', { d: d, fill: 'none', stroke: css('--g-one'), 'stroke-width': 2, 'stroke-linejoin': 'round', 'stroke-linecap': 'round' }));
    svg.appendChild(s('circle', { cx: x(last.t), cy: y(last[key]), r: 5, fill: css('--g-one'), stroke: css('--g-surface'), 'stroke-width': 2 }));
    svg.appendChild(text(x(last.t) + 9, y(last[key]) + 4, fmt(last[key]), { 'class': 'g-val' }));
    var span = t1 - t0, lab = function (t) { var dt = new Date(t * 1000); return span > 36 * 3600 ? dayLabel(dt.getFullYear() + '-' + ('0' + (dt.getMonth() + 1)).slice(-2) + '-' + ('0' + dt.getDate()).slice(-2)) : ('0' + dt.getHours()).slice(-2) + ':' + ('0' + dt.getMinutes()).slice(-2); };
    [t0, (t0 + t1) / 2, t1].forEach(function (t, i) { svg.appendChild(text(x(t), H - Bm + 16, lab(t), { 'text-anchor': i === 0 ? 'start' : i === 1 ? 'middle' : 'end' })); });
    var cross = s('line', { y1: T, y2: H - Bm, stroke: css('--g-ink2'), 'stroke-width': 1, visibility: 'hidden' }), dot = s('circle', { r: 5, fill: css('--g-one'), stroke: css('--g-surface'), 'stroke-width': 2, visibility: 'hidden' });
    var hit = s('rect', { 'class': 'g-hit', x: L, y: T, width: W - L - R, height: H - T - Bm });
    hit.addEventListener('mousemove', function (e) {
      var b = svg.getBoundingClientRect(), t = t0 + (t1 - t0) * ((e.clientX - b.left) / b.width * W - L) / (W - L - R), at = rows[0];
      rows.forEach(function (r) { if (r.t <= t) at = r; });
      cross.setAttribute('x1', x(at.t)); cross.setAttribute('x2', x(at.t)); cross.setAttribute('visibility', 'visible'); dot.setAttribute('cx', x(at.t)); dot.setAttribute('cy', y(at[key])); dot.setAttribute('visibility', 'visible');
      tipAt(k, e, '<b>' + new Date(at.t * 1000).toLocaleString() + '</b>' + row(css('--g-one'), words, fmt(at[key])));
    });
    hit.addEventListener('mouseleave', function () { cross.setAttribute('visibility', 'hidden'); dot.setAttribute('visibility', 'hidden'); k.tip.hidden = true; });
    svg.appendChild(cross); svg.appendChild(dot); svg.appendChild(hit);
    k.plot.insertBefore(svg, k.tip);
  }

  // ---- the commits a day on integration: a column a day
  function commits(G) {
    var k = card('g-commits'), days = G.commits || [];
    if (!fresh(k, JSON.stringify(days))) return;
    table(k, ['Day', 'Commits'], days.slice().reverse().map(function (d) { return [d.day, d.n]; }));
    var top = Math.max.apply(null, days.map(function (d) { return d.n; }).concat([0]));
    if (!top) return empty(k, 'No commit on integration in the last ' + days.length + ' days (as this checkout last fetched it).');
    var W = 480, H = 220, L = 40, R = 8, T = 14, Bm = 30, yt = ticks(top, 4), ymax = yt[yt.length - 1], slot = (W - L - R) / days.length, bw = Math.min(24, slot - 2);
    var y = function (v) { return T + (H - T - Bm) * (1 - v / ymax); }, svg = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img', 'aria-label': 'Commits a day on integration, the last ' + days.length + ' days' });
    grid(svg, yt, L, W - R, y);
    days.forEach(function (d, i) {
      var g = s('g'), x = L + i * slot + (slot - bw) / 2;
      if (d.n) g.appendChild(s('path', { 'class': 'g-mark', d: bar(x, y(d.n), bw, y(0) - y(d.n), 4), fill: css('--g-one') }));
      g.appendChild(s('rect', { 'class': 'g-hit', x: L + i * slot, y: T, width: slot, height: H - T - Bm }));
      hover(k, g, function () { return '<b>' + dayLabel(d.day) + '</b>' + row(css('--g-one'), 'commits', d.n); });
      svg.appendChild(g);
      if (new Date(d.day + 'T12:00:00').getDay() === 1) svg.appendChild(text(L + i * slot + slot / 2, H - Bm + 16, dayLabel(d.day), { 'text-anchor': 'middle' }));
    });
    k.plot.insertBefore(svg, k.tip);
  }

  // ---- the models by kind and how far along they are: a bar a kind, a shade a status
  function models(G) {
    var k = card('g-models'), list = G.models || [];
    if (!fresh(k, JSON.stringify(list))) return;
    if (k.legend && !k.legend.children.length) STATUS.forEach(function (st) { var b = el('button'); b.type = 'button'; b.title = 'The models that are: ' + st[1].toLowerCase(); var i = el('i'); i.style.background = st[2]; b.appendChild(i); b.appendChild(document.createTextNode(st[1]));
      b.addEventListener('click', function () { go('index.html#' + st[0]); }); k.legend.appendChild(b); });
    if (!list.length) return empty(k, 'The board was not built on this station, so there are no models to count here (build.py makes them).');
    var kinds = Object.keys(KIND).filter(function (c) { return list.some(function (m) { return m.kind === c; }); }), n = function (c, st) { var m = list.filter(function (x) { return x.kind === c && x.status === st; })[0]; return m ? m.n : 0; };
    table(k, ['Kind'].concat(STATUS.map(function (st) { return st[1]; })), kinds.map(function (c) { return [KIND[c]].concat(STATUS.map(function (st) { return n(c, st[0]); })); }));
    var W = 480, rowH = 40, L = 92, R = 40, H = kinds.length * rowH + 6, sum = function (c) { return STATUS.reduce(function (a, st) { return a + n(c, st[0]); }, 0); }, top = Math.max.apply(null, kinds.map(sum));
    var svg = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img', 'aria-label': 'Models by kind and status' });
    kinds.forEach(function (c, i) {
      var yy = 4 + i * rowH, x = L, live = STATUS.filter(function (st) { return n(c, st[0]); });
      svg.appendChild(text(L - 10, yy + 21, KIND[c], { 'text-anchor': 'end', 'class': 'g-lab' }));
      live.forEach(function (st, j) {
        var v = n(c, st[0]), w = (W - L - R) * v / top, g = s('g'), wd = Math.max(2, w - (j < live.length - 1 ? 2 : 0));
        g.appendChild(s('path', { 'class': 'g-mark', d: bar(x, yy + 6, wd, 22, j === live.length - 1 ? 4 : 0, 'right'), fill: st[2] }));
        g.appendChild(s('rect', { 'class': 'g-hit', x: x, y: yy, width: Math.max(w, 6), height: rowH }));
        hover(k, g, function () { return '<b>' + KIND[c] + '</b>' + row(st[2], st[1], v) + row('', 'of', sum(c)) + '<em>click: these models on the overview</em>'; }, 'index.html#' + st[0]);
        svg.appendChild(g); x += w;
      });
      svg.appendChild(text(x + 8, yy + 21, sum(c), { 'class': 'g-val' }));
    });
    k.plot.insertBefore(svg, k.tip);
  }

  // ---- the tiles: a number each, and where it has been
  function spark(svg, values, color) {
    if (!svg) return;                    // a page shows the tiles it has
    while (svg.firstChild) svg.firstChild.remove();
    if (values.length < 2) return;
    var top = Math.max.apply(null, values.concat([1])), W = 200, H = 34, d = '';
    values.forEach(function (v, i) { var x = W * i / (values.length - 1), y = H - 3 - (H - 8) * v / top; d += (i ? 'L' : 'M') + x.toFixed(1) + ',' + y.toFixed(1); });
    svg.setAttribute('viewBox', '0 0 ' + W + ' ' + H); svg.setAttribute('preserveAspectRatio', 'none');
    svg.appendChild(s('path', { d: d + 'L' + W + ',' + H + 'L0,' + H + 'z', fill: color, opacity: .12 })); svg.appendChild(s('path', { d: d, fill: 'none', stroke: color, 'stroke-width': 2, 'vector-effect': 'non-scaling-stroke' }));
  }
  function tiles(G, o) {
    var day = (G.history || []).filter(function (r) { return r.t >= Date.now() / 1000 - 86400; }), all = C.everyone(o), at = all.at.filter(function (x) { return x.w.state === 'working'; }).length;
    var Q = window.OwnerQueue, needs = Q && window.QUEUE ? Q.count(window.QUEUE) : 0, set = function (id, v) { var e = document.getElementById(id); if (e && e.textContent !== String(v)) e.textContent = v; };
    set('t-at', at); set('t-at-d', at ? all.at.filter(function (x) { return x.w.state === 'working' && x.w.kind === 'session'; }).length + ' sessions, the rest skills, agents and machines' : 'nobody is at work');
    spark(document.getElementById('t-at-s'), day.map(function (r) { return r.at; }), css('--g-one'));
    set('t-needs', needs); set('t-needs-d', needs ? 'things wait on you' : 'nothing waits on you');
    spark(document.getElementById('t-needs-s'), (G.history || []).map(function (r) { return r.needs; }), css('--g-lab'));
    set('t-calls', fmt(G.today || 0)); set('t-calls-d', fmt(G.week || 0) + ' in seven days');
    var byDay = {}; (G.hours || []).forEach(function (h) { byDay[h.h] = (G.rooms || []).reduce(function (n, r) { return n + (h[r] || 0); }, 0); });
    spark(document.getElementById('t-calls-s'), Object.keys(byDay).sort().map(function (h) { return byDay[h]; }), css('--g-shop'));
    var n = B ? B.count({ kind: 'page', id: 'all' }) : 0; set('t-notes', n); set('t-notes-d', n ? 'open, waiting for an answer' : 'none open: click anything to leave one');
  }

  function draw() {
    var G = window.GRAPHS, o = window.OPS;
    if (!G || !o) return;
    C.nowPill(o);
    var st = document.getElementById('g-stamp'), f = window.OwnerQueue ? window.OwnerQueue.fresh(window.BEAT, Date.now()) : { stale: false, text: '' };
    if (st) { st.textContent = f.stale ? f.text + ': the watcher has stopped (ops.py --watch 20), so the graphs stand still' : 'drawn from the reading of ' + String(window.BEAT || window.OPS_NOW || '').replace('T', ' ') + ', again every 20 s'; st.parentNode.classList.toggle('stale', !!f.stale); }
    tiles(G, o); rooms(G); lanes(G); line('g-needs', G, 'needs', 'waiting on you'); commits(G); models(G);
    document.documentElement.classList.add('g-ready');
  }
  var nb = document.getElementById('t-notes-b');
  if (nb) nb.addEventListener('click', function () { if (!B) return; B.open(B.all); if (B.docked()) go('#profile'); });
  if (B) B.onchange(function () { if (window.GRAPHS && window.OPS) tiles(window.GRAPHS, window.OPS); });
  C.live(draw);
})(typeof window !== 'undefined' ? window : this);
