// The office, counted: the week's numbers as things on a floor (graphs.html, and the control screen index.html).
// The owner's pick of 2026-10-07 ("every number a thing, you can turn it"); it took the place of the five graphs.
// A crate is so many tool calls of a branch this week (orange: today's), a frog sits at the desk of a branch somebody
// is at work on and sleeps by the others, a sheet is one decision of his, a shell is so many dollars of the relay's
// legs, a sandbag so many commits landed on integration. The numbers are data/graphs.js (src_graphs.py), which ops.py
// makes again on every read; the page reads it again every 20 seconds (crew.js).
// This file is the rules and the page: which branches stand on the floor, how many things a number is, where each
// stands (plan), the words of every label, and the same numbers as a plain list, which is what a screen reader gets
// and what the page shows when the browser gives no WebGL. countedscene.js draws it with three.js and knows no rule.
// The rules are plain functions (test_assetboard.py runs them under node). The tiles above the scene are drawn here too.
(function (root) {
  'use strict';
  // ---- words and numbers
  function fmt(n) { return String(Math.round(n || 0)).replace(/\B(?=(\d{3})+(?!\d))/g, ','); }
  function money(n) { return '$' + fmt(n); }
  function dayLabel(d) { var t = new Date(String(d).slice(0, 10) + 'T12:00:00'); return ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][t.getDay()] + ' ' + t.getDate(); }
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  function slug(b) { return 'room-' + String(b || '').replace(/[^a-z0-9]+/gi, '-'); }
  function plural(n, one, many) { return fmt(n) + ' ' + (n === 1 ? one : many || one + 's'); }

  // ---- how many things a number is
  // What one thing stands for: the smallest of the steps that keeps the largest number at `most` things or fewer, so
  // a week ten times as busy is the same floor with heavier crates, and the legend says which.
  var CRATE = [100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000], SHELL = [10, 20, 50, 100, 200, 500, 1000, 2000, 5000], BAG = [10, 20, 50, 100, 200, 500, 1000];
  var TALLEST = 48, SHELLS = 45, BAGS = 42, STACK = 80;          // crates in the highest stack, shells, bags, sheets in one pile
  function unit(top, steps, most) { for (var i = 0; i < steps.length; i++) if (top / steps[i] <= most) return steps[i]; return steps[steps.length - 1]; }
  // a stack of crates for a number: whole crates, and a part of one on top when what is left is worth showing (a
  // branch with a handful of calls is a sliver, not nothing and not a whole crate)
  function pieces(n, per) {
    var full = Math.floor(n / per), part = n / per - full;
    if (part >= 0.85) { full += 1; part = 0; } else if (part < 0.15) part = (!full && n > 0) ? 0.15 : 0;
    return { full: full, part: Math.round(part * 100) / 100 };
  }
  // shells and bags are whole: the nearest number of them, and one for anything at all
  function whole(n, per) { return n > 0 ? Math.max(1, Math.round(n / per)) : 0; }
  // which day each crate of a stack is, from the floor up: the day in which the middle of that crate's calls fell.
  // `days` is the branch's calls a day, oldest first, today the last: the crates of the last day are the orange ones.
  function bands(days, per, count) {
    var out = [], sum = 0, ends = (days || []).map(function (d) { sum += d; return sum; });
    for (var k = 0; k < count; k++) {
      var mid = Math.min(sum, (k + 0.5) * per), day = 0;
      while (day < ends.length - 1 && ends[day] < mid) day++;
      out.push(day);
    }
    return out;
  }

  // ---- who is at work on which branch now, from a reading of the floor (data/ops.js): {branch: [names]}
  function crew(o) {
    var at = {}, seen = {};
    function put(branch, id, name) { var k = id + '@' + branch; if (seen[k]) return; seen[k] = 1; (at[branch] = at[branch] || []).push(name); }
    ((o && o.lanes) || []).forEach(function (l) { (l.workers || []).forEach(function (w) { if (w.state === 'working') put(l.branch, w.id, w.name || w.kind); }); });
    ((o && o.roster) || []).forEach(function (r) { (r.busy || []).forEach(function (b) { put(b, r.id, r.name); }); });
    return at;
  }

  // ---- which branches stand on the floor: the busiest of the week, `max` of them. A branch somebody is at work on
  // now stands there too, whatever its week: it takes the place of the least busy branch nobody is on.
  function stations(G, o, max) {
    var at = crew(o), rows = ((G && G.lanes) || []).map(function (l) {
      return { branch: l.branch, name: short(l.branch), calls: l.calls, days: l.days || [], today: (l.days || [])[(l.days || []).length - 1] || 0, busy: l.busy || 0, crew: at[l.branch] || [] };
    });
    Object.keys(at).forEach(function (b) { if (!rows.some(function (r) { return r.branch === b; })) rows.push({ branch: b, name: short(b), calls: 0, days: [], today: 0, busy: 0, crew: at[b] }); });
    var by = function (a, b) { return b.calls - a.calls || (a.branch < b.branch ? -1 : 1); };
    rows.sort(by);
    var keep = rows.slice(0, max), out = rows.slice(max).filter(function (r) { return r.crew.length; });
    out.forEach(function (r) {
      for (var i = keep.length - 1; i >= 0; i--) if (!keep[i].crew.length) { keep[i] = r; return; }
    });
    return keep.sort(by);
  }

  // ---- everything the scene shows, as numbers of things
  function things(G, o, max) {
    G = G || {};
    var st = stations(G, o, max || 9), top = Math.max.apply(null, st.map(function (s) { return s.calls; }).concat([0])), crate = unit(top, CRATE, TALLEST);
    st.forEach(function (s) {
      var p = pieces(s.calls, crate), n = p.full + (p.part ? 1 : 0), day = bands(s.days, crate, n), last = Math.max(0, s.days.length - 1);
      s.full = p.full; s.part = p.part; s.day = day; s.hot = s.today > 0 ? day.filter(function (d) { return d === last; }).length : 0;
    });
    var week = (G.commits || []).slice(-7), landed = week.reduce(function (n, d) { return n + d.n; }, 0), bag = unit(landed, BAG, BAGS);
    var D = G.decisions || {}, R = G.relay || null, shell = R ? unit(R.usd, SHELL, SHELLS) : 0, floor = st.reduce(function (n, s) { return n + s.calls; }, 0);
    return {
      crate: crate, bag: bag, shell: shell, stations: st,
      calls: { week: G.week || 0, today: G.today || 0, floor: floor },
      sheets: { waiting: D.waiting || 0, closed: D.closed || 0, crew: D.crew || 0, asked: D.asked || 0 },
      bags: { n: whole(landed, bag), commits: landed, today: week.length ? week[week.length - 1].n : 0 },
      shells: R ? { n: whole(R.usd, shell), usd: R.usd, legs: R.legs, unpriced: R.unpriced || 0 } : null
    };
  }

  // ---- the words on the floor
  // a branch's name in the room a label has: the start and the end of a long one, since branches differ at both
  function brief(name, most) { most = most || 16; if (name.length <= most) return name; var a = Math.ceil((most - 1) / 2); return name.slice(0, a) + '…' + name.slice(name.length - (most - 1 - a)); }
  // the label over a stack: the week's number, the branch, and one short fact: today's calls when there are any, else
  // the hours it was busy in (the card of a picked branch and the list have both, and who is at work)
  function stationLabel(s, narrow) {
    return { n: fmt(s.calls), name: brief(s.name, narrow ? 11 : 16), small: s.today ? fmt(s.today) + ' today' : s.busy ? plural(s.busy, 'busy hour') : '', hot: s.today > 0, work: s.crew.length > 0 };
  }
  // the words under the table's piles, the sandbags and the shells; on a narrow floor the short ones (the list has the rest)
  function tags(T, narrow) {
    var S = T.shells;
    return {
      waiting: { n: fmt(T.sheets.waiting), small: T.sheets.waiting === 1 ? 'waits on you' : 'wait on you' },
      closed: { n: fmt(T.sheets.closed), small: narrow ? 'closed' : 'answered, closed' },
      crew: { n: fmt(T.sheets.crew), small: narrow ? 'on the crew' : T.sheets.crew === 1 ? 'waits on the crew' : 'wait on the crew' },
      bags: { n: fmt(T.bags.commits), small: 'commits landed' + (T.bags.today && !narrow ? ' · ' + fmt(T.bags.today) + ' today' : '') },
      shells: !S ? { n: '–', small: narrow ? 'no relay record' : 'no relay leg on record here' }
        : { n: money(S.usd), small: 'the relay · ' + plural(S.legs, 'leg') + (S.unpriced && !narrow ? ', ' + S.unpriced + ' without a cost' : '') }
    };
  }
  // what a thing is, in a line each, for the legend under the scene
  function legend(T) {
    return [
      { key: 'crate', text: '1 crate = ' + fmt(T.crate) + ' tool calls of a branch this week' },
      { key: 'hot', text: 'orange = today’s, lighter blue = later in the week' },
      { key: 'frog', text: 'a frog at a desk = somebody is at work there now' },
      { key: 'sheet', text: '1 sheet = 1 decision of yours' },
      { key: 'shell', text: T.shells ? '1 shell = ' + money(T.shell) + ' the relay’s legs cost (Claude’s figure, not a bill)' : 'no shells: this station has no record of a relay leg' },
      { key: 'bag', text: '1 sandbag = ' + fmt(T.bag) + ' commits landed on integration' }
    ];
  }
  // a branch that was picked: its week, day by day
  function card(s, days) {
    return {
      title: s.branch,
      head: plural(s.calls, 'tool call') + ' this week' + (s.busy ? ', busy in ' + plural(s.busy, 'hour') : ''),
      days: (days || []).map(function (d, i) { return { day: dayLabel(d), n: s.days[i] || 0, today: i === days.length - 1 }; }),
      crew: s.crew.length ? s.crew.length + ' at work now: ' + s.crew.join(', ') : 'nobody is at work on it now',
      href: 'floor.html#' + slug(s.branch)
    };
  }

  // ---- where each thing stands, in the floor's own units (x to the right, z towards the viewer, a crate is 0.8).
  // Wide: the branches in a shallow arc, the owner's table in front of them, the sandbags to the left of it, the
  // shells to the right. Narrow (a phone held upright): four rows, back to front: the three busiest branches, the
  // next three, the table, and the sandbags beside the shells. `side` is where a stack's label goes: above it, or,
  // in a front row with stacks behind it, on the floor in front of its frog, so no label lies on another row's.
  // `half` is how far the floor's things reach from its middle; `el` how high the eye is, `sweep` how far it drifts.
  function plan(n, narrow) {
    var st = [], i;
    if (narrow) {
      var rows = Math.max(1, Math.ceil(n / 3)), pitch = 6.2;
      for (i = 0; i < n; i++) {
        var r = Math.floor(i / 3), inRow = Math.min(3, n - r * 3);
        st.push({ x: ((i % 3) - (inRow - 1) / 2) * 5.6, z: -3 - (rows - 1 - r) * pitch, side: rows > 1 && r === rows - 1 ? 'below' : 'above' });
      }
      return { narrow: true, stations: st, rug: { x: 0, z: 5.4, w: 14.4, d: 3.2 }, tray: { x: -5.2, z: 5.4 }, closed: { x: 0, z: 5.4 }, crew: { x: 5.2, z: 5.4 },
               bags: { x: -6.2, z: 13.2, cols: 4 }, shells: { x: 1.9, z: 12.4, cols: 8 }, half: { w: 7.2, back: -3 - (rows - 1) * pitch - 1.4, front: 17.2 }, el: 0.8, sweep: 0.1 };
    }
    for (i = 0; i < n; i++) { var x = (i - (n - 1) / 2) * 3.8; st.push({ x: x, z: -4 + 0.02 * x * x, side: 'above' }); }
    return { narrow: false, stations: st, rug: { x: 0, z: 7.2, w: 13.4, d: 4.2 }, tray: { x: -4.6, z: 7.2 }, closed: { x: 0, z: 7.2 }, crew: { x: 4.6, z: 7.2 },
             bags: { x: -17.6, z: 5.2, cols: 7 }, shells: { x: 10.6, z: 6.4, cols: 9 }, half: { w: 19.4, back: -5.4, front: 11 }, el: 0.3, sweep: 0.3 };
  }

  // ---- the same numbers as a plain list: every number the floor shows, and the ones it has no room for.
  // A row is [what, how many, more, link].
  function lines(G, o, T) {
    G = G || {};
    var at = crew(o), days = (G.days || []), all = (G.lanes || []), named = all.slice(0, 12), rest = all.slice(12), out = [];
    var rows = named.map(function (l) {
      var by = (l.days || []).map(function (n, i) { return n ? dayLabel(days[i] ? days[i].day : '') + ': ' + fmt(n) : ''; }).filter(Boolean), today = (l.days || [])[(l.days || []).length - 1] || 0;
      return [short(l.branch), fmt(l.calls), [fmt(today) + ' today', 'busy in ' + plural(l.busy || 0, 'hour'), (at[l.branch] || []).length ? (at[l.branch] || []).length + ' at work now' : '', by.join(', ')].filter(Boolean).join(' · '), 'floor.html#' + slug(l.branch)];
    });
    var listed = all.reduce(function (n, l) { return n + l.calls; }, 0);
    if (rest.length) rows.push(['the other ' + plural(rest.length, 'branch', 'branches'), fmt(rest.reduce(function (n, l) { return n + l.calls; }, 0)), '']);
    if ((G.week || 0) - listed > 0) rows.push(['in no checkout of a branch', fmt(G.week - listed), 'sessions started elsewhere, or in a checkout that is gone']);
    rows.push(['all the work', fmt(G.week || 0), fmt(G.today || 0) + ' today']);
    out.push({ key: 'work', head: 'The work, by branch', note: 'Tool calls this week on this station. A branch has the work of the checkout that is on it now.', rows: rows });
    var commits = {}, relay = {};
    (G.commits || []).forEach(function (d) { commits[d.day] = d.n; });
    ((G.relay && G.relay.days) || []).forEach(function (d) { relay[d.day] = d; });
    out.push({ key: 'days', head: 'The week, day by day', note: '', rows: days.map(function (d) {
      var r = relay[d.day];
      return [dayLabel(d.day), plural(d.calls, 'tool call'), [plural(commits[d.day] || 0, 'commit') + ' landed', r ? money(r.usd) + ' relay, ' + plural(r.legs, 'leg') : ''].filter(Boolean).join(' · ')];
    }) });
    out.push({ key: 'decisions', head: 'Your decisions', note: '', rows: [
      [T.sheets.waiting === 1 ? 'waits on you' : 'wait on you', fmt(T.sheets.waiting), 'the number "Needs you" shows', 'decide.html'],
      ['answered by you, waiting on the crew', fmt(T.sheets.crew), 'no session has taken them up yet', 'decide.html'],
      ['answered and closed this week', fmt(T.sheets.closed), ''],
      ['put to you this week', fmt(T.sheets.asked), '']] });
    var month = (G.commits || []).reduce(function (n, d) { return n + d.n; }, 0);
    out.push({ key: 'landed', head: 'Landed on integration', note: '', rows: [['commits this week', fmt(T.bags.commits), fmt(T.bags.today) + ' today'], ['commits in ' + (G.commits || []).length + ' days', fmt(month), '']] });
    var S = T.shells;
    out.push({ key: 'relay', head: 'The relay', note: S ? 'What Claude printed as the cost of each leg, added up. On the plan that is a yardstick, not a bill.' + (S.unpriced ? ' A leg without a cost on record is counted as a leg and not guessed at.' : '')
      : 'This station has no record of a relay leg, so no cost is shown and there are no shells on the floor.',
      rows: S ? [['cost of its legs this week', money(S.usd), plural(S.legs, 'leg') + (S.unpriced ? ', ' + S.unpriced + ' without a cost on record' : '')]] : [] });
    return out;
  }

  var pure = { fmt: fmt, money: money, dayLabel: dayLabel, short: short, brief: brief, unit: unit, pieces: pieces, whole: whole, bands: bands, crew: crew, stations: stations, things: things, stationLabel: stationLabel,
               tags: tags, legend: legend, card: card, plan: plan, lines: lines, CRATE: CRATE, SHELL: SHELL, BAG: BAG, TALLEST: TALLEST, SHELLS: SHELLS, BAGS: BAGS, STACK: STACK };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }
  root.Counted = pure;

  // ================= the page =================
  var C = window.Crew, B = window.Board, page = document.getElementById('graphs');
  if (!C || !page) return;
  var el = C.el, NS = 'http://www.w3.org/2000/svg';
  function $(id) { return document.getElementById(id); }
  function css(name) { return getComputedStyle(page).getPropertyValue(name).trim(); }
  var calm = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  // ---- the tiles: a number each, and where it has been
  function spark(svg, values, color) {
    if (!svg) return;                    // a page shows the tiles it has
    while (svg.firstChild) svg.firstChild.remove();
    if (values.length < 2) return;
    var top = Math.max.apply(null, values.concat([1])), W = 200, H = 34, d = '';
    values.forEach(function (v, i) { var x = W * i / (values.length - 1), y = H - 3 - (H - 8) * v / top; d += (i ? 'L' : 'M') + x.toFixed(1) + ',' + y.toFixed(1); });
    svg.setAttribute('viewBox', '0 0 ' + W + ' ' + H); svg.setAttribute('preserveAspectRatio', 'none');
    var a = document.createElementNS(NS, 'path'), b = document.createElementNS(NS, 'path');
    a.setAttribute('d', d + 'L' + W + ',' + H + 'L0,' + H + 'z'); a.setAttribute('fill', color); a.setAttribute('opacity', '.12');
    b.setAttribute('d', d); b.setAttribute('fill', 'none'); b.setAttribute('stroke', color); b.setAttribute('stroke-width', '2'); b.setAttribute('vector-effect', 'non-scaling-stroke');
    svg.appendChild(a); svg.appendChild(b);
  }
  function tiles(G, o) {
    var day = (G.history || []).filter(function (r) { return r.t >= Date.now() / 1000 - 86400; }), all = C.everyone(o), at = all.at.filter(function (x) { return x.w.state === 'working'; }).length;
    var Q = window.OwnerQueue, needs = Q && window.QUEUE ? Q.count(window.QUEUE) : 0, set = function (id, v) { var e = $(id); if (e && e.textContent !== String(v)) e.textContent = v; };
    set('t-at', at); set('t-at-d', at ? all.at.filter(function (x) { return x.w.state === 'working' && x.w.kind === 'session'; }).length + ' sessions, the rest skills, agents and machines' : 'nobody is at work');
    spark($('t-at-s'), day.map(function (r) { return r.at; }), css('--g-one'));
    set('t-needs', needs); set('t-needs-d', needs ? 'things wait on you' : 'nothing waits on you');
    spark($('t-needs-s'), (G.history || []).map(function (r) { return r.needs; }), css('--g-lab'));
    set('t-calls', fmt(G.today || 0)); set('t-calls-d', fmt(G.week || 0) + ' in seven days');
    spark($('t-calls-s'), (G.hours || []).map(function (h) { return (G.rooms || []).reduce(function (n, r) { return n + (h[r] || 0); }, 0); }), css('--g-shop'));
    var n = B ? B.count({ kind: 'page', id: 'all' }) : 0; set('t-notes', n); set('t-notes-d', n ? 'open, waiting for an answer' : 'none open: click anything to leave one');
  }

  // ---- the floor, its legend, the card of a picked branch and the list
  var stage = $('o-stage'), scene = null, shown = '', picked = null, last = null, asked = 0;
  // a stage taller than it is wide (a phone held upright: board.css) gets the narrow floor and six branches
  function narrow() { return !!stage && stage.clientHeight > 0 && stage.clientWidth / stage.clientHeight < 1.1; }
  function flat(why) {          // no WebGL here: the list is the page
    if (!stage) return;
    stage.classList.add('o-flat');
    var p = $('o-why'); if (p) p.textContent = why;
    var d = $('o-list-box'); if (d) d.open = true;
    var ctl = $('o-ctl'); if (ctl) ctl.hidden = true;
  }
  function showCard(T) {
    var box = $('o-card'); if (!box) return;
    var s = picked && T.stations.filter(function (x) { return x.branch === picked; })[0];
    if (!s) { box.hidden = true; picked = null; return; }
    var k = card(s, ((window.GRAPHS || {}).days || []).map(function (d) { return d.day; })), sig = JSON.stringify(k);
    box.hidden = false;
    if (box.dataset.sig === sig) return;
    box.dataset.sig = sig; box.innerHTML = '';
    var x = el('button', 'o-x', '×'); x.type = 'button'; x.setAttribute('aria-label', 'Close'); x.addEventListener('click', function () { picked = null; box.hidden = true; });
    box.appendChild(x); box.appendChild(el('b', null, k.title)); box.appendChild(el('span', null, k.head));
    var row = el('div', 'o-days'); k.days.forEach(function (d) { var c = el('span', d.today ? 'o-today' : (d.n ? '' : 'o-none')); c.appendChild(el('i', null, d.day)); c.appendChild(el('b', null, fmt(d.n))); row.appendChild(c); });
    box.appendChild(row); box.appendChild(el('span', null, k.crew));
    var a = el('a', null, 'its room on the branches page →'); a.href = k.href; box.appendChild(a);
  }
  function list(G, o, T) {
    var host = $('o-list'); if (!host) return;
    var secs = lines(G, o, T), sig = JSON.stringify(secs);
    if (host.dataset.sig === sig) return;
    host.dataset.sig = sig; host.innerHTML = '';
    secs.forEach(function (s) {
      var sec = el('section', 'o-sec o-' + s.key), t = el('table'), cap = el('caption', null, s.head), tb = el('tbody');
      t.appendChild(cap);
      s.rows.forEach(function (r) {
        var tr = el('tr'), th = el('th'); th.scope = 'row';
        if (r[3]) { var a = el('a', null, r[0]); a.href = r[3]; th.appendChild(a); } else th.textContent = r[0];
        tr.appendChild(th); tr.appendChild(el('td', 'o-n', r[1])); tr.appendChild(el('td', 'o-more', r[2] || '')); tb.appendChild(tr);
      });
      t.appendChild(tb); sec.appendChild(t);
      if (s.note) sec.appendChild(el('p', 'o-note', s.note));
      host.appendChild(sec);
    });
  }
  function key(T) {
    var host = $('o-legend'); if (!host) return;
    var L = legend(T), sig = JSON.stringify(L);
    if (host.dataset.sig === sig) return;
    host.dataset.sig = sig; host.innerHTML = '';
    L.forEach(function (l) { var s = el('span', 'o-k o-k-' + l.key); s.appendChild(el('i')); s.appendChild(document.createTextNode(l.text)); host.appendChild(s); });
  }
  function floor(G, o) {
    if (!stage) return;
    var T = things(G, o, narrow() ? 6 : 9), P = plan(T.stations.length, narrow());
    last = T;
    key(T); list(G, o, T); showCard(T);
    if (scene === null) {
      var S = window.CountedScene;
      scene = S ? S.mount(stage, { calm: calm, frogs: window.FROGTEX || {}, pick: function (branch) { picked = branch; showCard(last); }, lost: function () { flat('The browser took the 3D picture away. The same numbers are in the list below.'); } }) : false;
      // a browser whose graphics were busy may refuse once and give the next time: asked twice before the list stands alone
      if (!scene && S && !asked) { asked = 1; scene = null; setTimeout(function () { if (window.GRAPHS && window.OPS) floor(window.GRAPHS, window.OPS); }, 1500); return; }
      if (!scene) flat('This browser gives no WebGL, so the floor is not drawn. The same numbers are in the list below.');
    }
    if (!scene) return;
    var labels = { stations: T.stations.map(function (s) { return stationLabel(s, P.narrow); }), tags: tags(T, P.narrow) }, sig = JSON.stringify([T, P.narrow]);
    if (sig === shown) return;
    shown = sig;
    scene.set(T, P, labels);
  }

  function draw() {
    var G = window.GRAPHS, o = window.OPS;
    if (!G || !o) return;
    C.nowPill(o);
    var st = $('g-stamp'), f = window.OwnerQueue ? window.OwnerQueue.fresh(window.BEAT, Date.now()) : { stale: false, text: '' };
    if (st) { st.textContent = f.stale ? f.text + ': the watcher has stopped (ops.py --watch 20), so the numbers stand still' : 'counted from the reading of ' + String(window.BEAT || window.OPS_NOW || '').replace('T', ' ') + ', again every 20 s'; st.parentNode.classList.toggle('stale', !!f.stale); }
    tiles(G, o); floor(G, o);
    document.documentElement.classList.add('g-ready');
  }
  var nb = $('t-notes-b');
  if (nb) nb.addEventListener('click', function () { if (!B) return; B.open(B.all); if (B.docked()) { var p = $('profile'); if (p) p.scrollIntoView({ behavior: calm ? 'auto' : 'smooth' }); } });
  if (B) B.onchange(function () { if (window.GRAPHS && window.OPS) tiles(window.GRAPHS, window.OPS); });
  // the buttons under the floor: what a drag does, for a keyboard and for whoever would rather click
  var ctl = $('o-ctl');
  if (ctl) ctl.addEventListener('click', function (e) { var b = e.target.closest ? e.target.closest('button[data-do]') : null; if (b && scene) scene.act(b.getAttribute('data-do')); });
  // a phone turned, or a window made narrow, is another floor
  var again = 0;
  window.addEventListener('resize', function () { clearTimeout(again); again = setTimeout(function () { if (window.GRAPHS && window.OPS) floor(window.GRAPHS, window.OPS); }, 200); });
  C.live(draw);
})(typeof window !== 'undefined' ? window : this);
