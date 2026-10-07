// The task board drawn: on tasks.html every task in its group (#t-board), on the control screen the first few under
// the owner's decisions (#tasks-cards). The rows and their words are tasks.js's; a click opens board.js's panel with
// what the task is, its picture, and the two things he can say. His click is a note: the row shows it at once
// (Board.onchange), and tasks.py takes it up on its next read.
(function () {
  'use strict';
  var T = window.Tasks, C = window.Crew, Bd = window.Board;
  if (!T) return;
  var full = document.getElementById('t-board'), over = document.getElementById('tasks-cards');
  if (!full && !over) return;
  var MOST_OVER = 6;
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  // the texts of his open notes about a task, so a click shows before the next reading
  function saidOf(r) {
    if (!Bd) return [];
    return Bd.notes({ kind: 'queue', id: 'task: ' + r.id }).filter(function (n) { return n.state !== 'done'; }).map(function (n) { return n.text; });
  }
  // One subject per task, kept and brought up to date on every draw, never made anew: the panel that is open on a
  // task holds this very object, so when a reading says the task is queued the panel says so too, and its two
  // buttons go (a panel left with what the row said when it was clicked offered them again: seen in the trial).
  var subjects = {};
  function subjectOf(g, raw) {
    var fresh = T.subject(raw, g.label, saidOf(raw)), s = subjects[raw.id] || (subjects[raw.id] = {}), k;
    for (k in s) if (!(k in fresh)) delete s[k];
    for (k in fresh) s[k] = fresh[k];
    return s;
  }
  function rowEl(g, r, raw) {
    var a = el('div', 'k-qrow k-theirs t-row t-' + r.state + (r.shot ? ' t-shown' : '')); a.title = r.tip || r.top;
    if (r.shot) { var i = el('img', 'k-qshot'); i.loading = 'lazy'; i.alt = ''; i.src = r.shot; a.appendChild(i); }
    var what = el('span', 'k-take-what');
    what.appendChild(el('span', 'k-take-top', r.top));
    if (r.sub && r.sub !== r.top) what.appendChild(el('span', 't-sub', r.sub));
    var sub = el('span', 'k-take-sub'); r.chips.forEach(function (c, k) { sub.appendChild(el('span', 'k-qchip' + (r.state === 'queued' && !k ? ' t-state' : ''), c)); });
    what.appendChild(sub); a.appendChild(what);
    if (!Bd) return a;
    var subj = subjectOf(g, raw), open = function () { Bd.open(subj); };
    a.classList.add('b-click'); a.tabIndex = 0; a.setAttribute('role', 'button');
    a.addEventListener('click', open); a.addEventListener('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); open(); } });
    a.appendChild(Bd.button(subj));
    return a;
  }
  function changed(into, sig) { if (into.dataset.sig === sig) return false; into.dataset.sig = sig; into.innerHTML = ''; return true; }
  function draw() {
    var data = window.TASKS || null, gs = T.groups(data, saidOf), sig = JSON.stringify(gs.map(function (g) { return [g.key, g.rows]; }));
    if (Bd) gs.forEach(function (g) { g.raw.forEach(function (raw) { subjectOf(g, raw); }); });
    // the drawer that is open on a task is drawn again from what this reading says (board.js draws it again only when
    // the notes are read, which is another clock: without this it trailed the rows by a reading)
    var drawer = document.querySelector('.b-panel:not(.b-docked):not([hidden])'), on = drawer && /^queue\|task: (.+)$/.exec(drawer.dataset.id || '');
    if (Bd && on && subjects[on[1]] && !draw.busy) { draw.busy = true; try { Bd.open(subjects[on[1]]); } finally { draw.busy = false; } }
    if (full) {
      var cnt = document.getElementById('t-count'); if (cnt) cnt.textContent = T.head(data, gs);
      var foot = document.getElementById('t-foot');
      if (foot && data) foot.textContent = 'A task is listed once nothing has touched it for ' + T.span(data.stale_minutes) + '; a capture from the game at once. Read on ' + T.heard(data, Date.now() / 1000) + '.'
        + (data.dropped ? ' ' + data.dropped + ' you took off the list.' : '');
      if (changed(full, sig)) {
        if (!gs.length) full.appendChild(el('p', 't-none', data ? 'Nothing is left unfinished. A task shows here when an agent stops before the end and nobody comes back to it, and when you press F10 in the game.' : 'The tasks have not been read yet.'));
        gs.forEach(function (g) {
          var sec = el('section', 't-group t-' + g.key), h = el('h3', 't-h'); h.appendChild(document.createTextNode(g.label + ' ')); h.appendChild(el('span', 't-n', String(g.rows.length))); sec.appendChild(h);
          var rows = el('div', 't-rows'); g.rows.forEach(function (r, k) { rows.appendChild(rowEl(g, r, g.raw[k])); }); sec.appendChild(rows);
          full.appendChild(sec);
        });
      }
    }
    if (over) {
      var home = document.getElementById('tasks'), all = []; gs.forEach(function (g) { g.rows.forEach(function (r, k) { all.push([g, r, g.raw[k]]); }); });
      if (home) home.hidden = !all.length;
      if (changed(over, sig)) {
        var card = el('article', 'k-qcard t-card'), hd = el('header'); hd.appendChild(el('b', null, 'Waiting for an agent')); hd.appendChild(el('span', 'k-qn', String(T.count(gs, 'left')))); card.appendChild(hd);
        var list = el('div', 't-rows'); all.slice(0, MOST_OVER).forEach(function (x) { list.appendChild(rowEl(x[0], x[1], x[2])); }); card.appendChild(list);
        if (all.length > MOST_OVER) { var more = el('a', 'k-qall', 'All ' + all.length + ' on the task board'); more.href = 'tasks.html'; card.appendChild(more); }
        over.appendChild(card);
      }
    }
  }
  if (C && C.live) C.live(draw); else draw();
  if (Bd && Bd.onchange) Bd.onchange(draw);
})();
