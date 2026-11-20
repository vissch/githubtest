// The priority list (priorities.html, and its first rows under the house on the control screen): data/priorities.js
// (src_priorities.py) as rows, in the order the work comes, each with why it stands there. Read-only (the owner,
// 2026-10-12): a row leads to where the thing is. One exception (the owner, 2026-10-21): a unit of the relay's queue has
// Move up and Move down, which send its new number to the listener (priomove.py). The words are pure functions, so
// test_priorities.py runs them under node; the drawing is below them.
(function (root) {
  var TOP = 8;                                  // rows under the house
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  function n(k, one, many) { return k + ' ' + (k === 1 ? one : many); }
  // the rows of one view: all, one band, or the rows whose words hold what he typed
  function rows(P, band, find) {
    var f = String(find || '').toLowerCase().trim();
    return ((P || {}).rows || []).filter(function (r) {
      if (band && band !== 'all' && r.band !== band) return false;
      return !f || [r.title, r.about, r.name, r.why, r.lane, (r.chips || []).join(' ')].join(' ').toLowerCase().indexOf(f) >= 0;
    });
  }
  // the first rows for the control screen: everything of his, then what is worked now, then what the relay may take
  // next (the first of each kind of work), then the rest in order, until the place is full
  function top(P, most) {
    var all = (P || {}).rows || [], k = most || TOP, out = [], seen = {};
    function add(r) { if (out.length < k && !seen[r.id]) { seen[r.id] = 1; out.push(r); } }
    all.filter(function (r) { return r.band === 'yours'; }).forEach(add);
    all.filter(function (r) { return r.band === 'now' && r.kind === 'relay unit'; }).forEach(add);
    all.filter(function (r) { return r.band === 'next' && r.first; }).forEach(add);
    all.filter(function (r) { return r.band === 'land'; }).forEach(add);
    all.filter(function (r) { return r.band === 'next' || r.band === 'left'; }).forEach(add);
    return out.sort(function (a, b) { return a.rank - b.rank; });
  }
  // the line over the list: how much there is, and of what
  function count(P) {
    var b = (P || {}).bands || [], all = b.reduce(function (s, x) { return s + x.n; }, 0), y = (b.filter(function (x) { return x.id === 'yours'; })[0] || {}).n || 0;
    if (!P) return 'reading the list…';
    if (!all) return 'Nothing is listed.';
    return n(all, 'thing', 'things') + ' to do' + (y ? ', ' + y + ' of them yours' : ', none of them yours');
  }
  // how the relay takes its queue, in his words: it is not simply top to bottom
  function relay(P) {
    var r = (P || {}).relay || {}, s = r.shares || {}, k = r.kinds || {};
    if (!r.waiting) return '';
    return 'The relay does not go straight down this list. It splits its day over three kinds of work (finishing ' + (s.finish || 0) + ' %, fixing ' + (s.fix || 0) +
      ' %, looking for faults ' + (s.critique || 0) + ' %) and takes the lowest number of the kind that has had least; every ' + ['', '', 'second', 'third', 'fourth', 'fifth'][r.wide_every || 3] +
      ' turn it takes the part of the game that waited longest instead. ' + n(r.waiting, 'unit waits', 'units wait') + ': ' +
      ['finish', 'fix', 'critique'].filter(function (x) { return k[x]; }).map(function (x) { return k[x] + ' ' + x; }).join(', ') + '. Read off the board ' + (r.as_of || 'at an unknown time') + '.';
  }
  // ---- moving a unit of the relay's queue: up = on the top, down = on the bottom (0 to 99, the lower runs first)
  function queued(rows) { return (rows || []).filter(function (r) { return r.band === 'next' && r.kind === 'relay unit'; }); }
  // the numbers a unit would get: {up, down}, each null when it is there already (or alone)
  function targets(rows, id) {
    var q = queued(rows), me = q.filter(function (r) { return r.name === id; })[0], others = q.filter(function (r) { return r.name !== id; });
    if (!me || !others.length) return { up: null, down: null };
    var ns = others.map(function (r) { return r.n; }), lo = Math.min.apply(null, ns), hi = Math.max.apply(null, ns), up = Math.max(0, lo - 1), down = Math.min(99, hi + 1);
    return { up: me.n >= lo && up !== me.n ? up : null, down: me.n <= hi && down !== me.n ? down : null };
  }
  // the rows as they stand with his moves the board has not shown yet: the unit's number, its place among the relay's rows (the
  // relay sorts by number, then by name) and the rank those rows share out in the same order
  function withMoves(rows, pend) {
    var ids = Object.keys(pend || {});
    if (!ids.length) return rows;
    var out = (rows || []).map(function (r) {
      var m = r.band === 'next' && r.kind === 'relay unit' ? pend[r.name] : null;
      if (!m) return r;
      var c = {}; Object.keys(r).forEach(function (k) { c[k] = r[k]; });
      c.n = m.n; c.why = String(r.why || '').replace(/^priority \d+( \(none set, so the usual 50\))?/, 'priority ' + m.n); c.chips = (r.chips || []).slice(); c.chips[0] = 'priority ' + m.n; c.moved = true;
      return c;
    });
    var at = [], mine = [];
    out.forEach(function (r, i) { if (r.band === 'next' && r.kind === 'relay unit') { at.push(i); mine.push(r); } });
    var ranks = mine.map(function (r) { return r.rank; }).sort(function (a, b) { return a - b; });
    mine.sort(function (a, b) { return a.n - b.n || (a.name < b.name ? -1 : a.name > b.name ? 1 : 0); });
    mine.forEach(function (r, i) { var c = {}; Object.keys(r).forEach(function (k) { c[k] = r[k]; }); c.rank = ranks[i]; at.length && (out[at[i]] = c); });
    return out;
  }
  // which of his moves the board has caught up with (or that are ten minutes old): those stop standing over the rows
  function settleMoves(rows, pend, now) {
    var by = {}; queued(rows).forEach(function (r) { by[r.name] = r; });
    var left = {}; Object.keys(pend || {}).forEach(function (k) { if (by[k] && by[k].n !== pend[k].n && now - pend[k].at < 600000) left[k] = pend[k]; });
    return left;
  }
  // what the line over the list says when a move is saved
  function moved(title, to, dir, rows, id) {
    var others = queued(rows).filter(function (r) { return r.name !== id; }), tie = others.some(function (r) { return r.n === to; });
    return title + ': priority ' + to + ' on the board, ' + (dir === 'up' ? 'on the top' : 'on the bottom') + (tie ? ' (another unit has the same number: the relay takes those by name)' : '') + '.';
  }
  var api = { queued: queued, targets: targets, withMoves: withMoves, settleMoves: settleMoves, moved: moved, TOP: TOP, short: short, n: n, rows: rows, top: top, count: count, relay: relay };
  if (typeof module !== 'undefined' && module.exports) { module.exports = api; return; }
  root.Priorities = api;

  // ---- the drawing
  var C = root.Crew;
  if (!C || typeof document === 'undefined') return;
  var el = C.el, $ = function (id) { return document.getElementById(id); };
  var band = 'all', find = '', sig = {}, pend = {};
  function say(text, bad) { var s = $('pr-say'); if (!s) return; s.hidden = !text; s.textContent = text || ''; s.classList.toggle('pr-bad', !!bad); }
  // what is sent to the listener (the box the notes use): plain text, so the browser asks no question first
  function post(path, body) {
    var b = root.NOTEBOX && root.NOTEBOX.url ? root.NOTEBOX : null;
    if (!b) return Promise.reject(new Error('the page does not know where the note box is (ops.py --watch runs it)'));
    body.key = b.key;
    return fetch(b.url + path, { method: 'POST', headers: { 'Content-Type': 'text/plain' }, body: JSON.stringify(body) })
      .then(function (r) { return r.json(); }).then(function (d) { if (!d.ok) throw new Error(d.why || 'refused'); return d; },
        function (e) { throw new Error(e.message === 'Failed to fetch' ? 'the note box does not answer (it runs while ops.py --watch runs)' : e.message); });
  }
  function move(r, dir) {
    var P = root.PRIORITIES, to = targets(P.rows, r.name)[dir];
    if (to == null) return;
    pend[r.name] = { n: to, at: Date.now(), busy: true }; say('Moving ' + r.title + '…'); draw();
    post('/priority/move', { id: r.name, to: to }).then(function () {
      pend[r.name].busy = false; say(moved(r.title, to, dir, P.rows, r.name)); draw();
    }, function (e) { delete pend[r.name]; say('Not moved: ' + e.message, true); draw(); });
  }
  function changed(key, s) { if (sig[key] === s) return false; sig[key] = s; return true; }
  function row(r, withBand, names, rows) {
    var mv = !!rows && r.band === 'next' && r.kind === 'relay unit';         // the relay's units carry the move buttons: the row is no link then, its title is
    var a = el(r.link && !mv ? 'a' : 'div', 'pr-row pr-b-' + r.band + (r.first && r.band === 'next' ? ' pr-first' : '') + (mv ? ' pr-has-move' : ''));
    if (r.link && !mv) a.href = r.link;
    a.appendChild(el('span', 'pr-rank', String(r.rank)));
    var what = el('span', 'pr-what');
    if (mv && r.link) { var tl = el('a', 'pr-title', r.title); tl.href = r.link; what.appendChild(tl); } else what.appendChild(el('b', 'pr-title', r.title));
    if (r.about) what.appendChild(el('span', 'pr-about', r.about));
    what.appendChild(el('span', 'pr-why', r.why));
    var chips = el('span', 'pr-chips');
    if (withBand) chips.appendChild(el('span', 'k-qchip pr-band', names[r.band] || r.band));
    (r.chips || []).forEach(function (c) { chips.appendChild(el('span', 'k-qchip', c)); });
    if (r.lane) chips.appendChild(el('span', 'k-qchip pr-lane', short(r.lane)));
    if (r.name && r.name !== r.title && r.name !== short(r.lane)) chips.appendChild(el('span', 'k-qchip pr-id', r.name));
    what.appendChild(chips);
    a.appendChild(what);
    if (mv) {
      var box = el('span', 'pr-move'), to = targets(rows, r.name), busy = !!(pend[r.name] && pend[r.name].busy);
      [['up', 'Move up'], ['down', 'Move down']].forEach(function (d) {
        var b = el('button', 'pr-mv', d[1]); b.type = 'button'; b.disabled = busy || to[d[0]] == null;
        b.title = to[d[0]] == null ? 'It is there already' : 'Set its priority to ' + to[d[0]] + ' (' + (d[0] === 'up' ? 'the top' : 'the bottom') + ')';
        b.onclick = function () { move(r, d[0]); };
        box.appendChild(b);
      });
      a.appendChild(box);
    }
    return a;
  }
  function names(P) { var o = {}; (P.bands || []).forEach(function (b) { o[b.id] = b.name; }); return o; }
  // under the house: the first rows, and the way to the whole list
  function drawTop(P) {
    var home = $('pr-rows'), sec = $('priorities');
    if (!home || !sec) return;
    var mine = top(P), nm = names(P);
    sec.hidden = !mine.length;
    if (!changed('top', JSON.stringify(mine))) return;
    home.textContent = '';
    mine.forEach(function (r) { home.appendChild(row(r, true, nm)); });
    var all = (P.rows || []).length, k = $('pr-n'), more = $('pr-all');
    if (k) k.textContent = '· the first ' + mine.length + ' of ' + all;
    if (more) more.textContent = 'all ' + all + ' →';
  }
  // the page of its own: a chip a band, a box to find a word, the bands one under the other
  function drawPage(P) {
    var home = $('pr-board');
    if (!home) return;
    var cnt = $('pr-count'); if (cnt) cnt.textContent = count(P);
    var warn = $('pr-warn'); if (warn) { warn.hidden = !(P.wrong || []).length; warn.textContent = (P.wrong || []).join(' '); }
    var foot = $('pr-foot'); if (foot) foot.textContent = relay(P);
    var bar = $('pr-bands');
    if (bar && changed('bar', JSON.stringify(P.bands) + band)) {
      bar.textContent = '';
      [{ id: 'all', name: 'Everything', n: (P.rows || []).length }].concat(P.bands || []).forEach(function (b) {
        var t = el('button', 'pr-tab' + (b.id === band ? ' on' : '') + (b.n ? '' : ' none'), b.name); t.type = 'button'; t.setAttribute('aria-pressed', b.id === band ? 'true' : 'false');
        t.appendChild(el('span', 'pr-tab-n', String(b.n)));
        t.onclick = function () { band = b.id; draw(); };
        bar.appendChild(t);
      });
    }
    pend = settleMoves(P.rows, pend, Date.now());
    var shown = withMoves(P.rows, pend), mine = rows({ rows: shown }, band, find);
    if (!changed('page', JSON.stringify(mine) + band + JSON.stringify(pend))) return;
    home.textContent = '';
    if (!mine.length) { home.appendChild(el('p', 'pr-none', find ? 'No row holds "' + find + '".' : 'Nothing here.')); return; }
    (P.bands || []).forEach(function (b) {
      var of = mine.filter(function (r) { return r.band === b.id; });
      if (!of.length) return;
      var g = el('section', 'pr-group pr-g-' + b.id), h = el('h3', 'pr-h', b.name);
      h.appendChild(el('span', 'pr-h-n', String(of.length))); h.appendChild(el('span', 'pr-h-says', b.says));
      g.appendChild(h);
      var list = el('div', 'pr-list');
      of.forEach(function (r) { list.appendChild(row(r, false, {}, shown)); });
      g.appendChild(list);
      home.appendChild(g);
    });
  }
  function draw() { var P = root.PRIORITIES; if (!P) return; drawTop(P); drawPage(P); if (C.nowPill && root.OPS) C.nowPill(root.OPS); }
  var box = $('pr-find');
  if (box) box.oninput = function () { find = box.value; draw(); };
  C.live(draw);
})(typeof window !== 'undefined' ? window : this);
