// What a click opens, on every page: a panel about the thing that was clicked (a worker, a branch, a question, a
// model, a bar of a graph) with where it leads (its branch, the models it touches) and the owner's notes about it,
// and a box to write a new one. A note goes to the listener ops.py --watch keeps on this machine (data/notebox.js
// says where, and holds the key it asks for); notes.py writes it as a file every session reads, and an answer comes
// back in data/notes.js, which is read again every 20 seconds. When no listener answers, the note is kept in this
// browser and sent when one does: the page says so, it never pretends a note was saved.
// On the control screen (index.html) Board.dock() gives the panel a place in the page that is always there: the
// profile of the worker the house has selected, with the last picture or film it had in its hands. That place is the
// workers' and nobody else's (the owner, 2026-10-06: "the screen to the right of the house should be exclusively for
// agents"): a row of the queue, a branch, the notes open in the drawer over the page, and the profile stays as it was.
// The matching and the counting are plain functions (test_assetboard.py runs them under node).
(function (root) {
  'use strict';
  function slug(b) { return 'room-' + String(b || '').replace(/[^a-z0-9]+/gi, '-'); }
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  // whether a note is about a subject: the same model, the same branch, or the very thing
  function about(n, s) {
    if (!s) return false;
    if (s.kind === 'page' && s.id === 'all') return true;
    if (s.asset && n.asset === s.asset) return true;
    if (s.lane && n.lane === s.lane) return true;
    return n.kind === s.kind && !!s.id && n.about === s.id;
  }
  // the notes about a subject, the open ones first, each group newest first
  function pick(notes, s) {
    return (notes || []).filter(function (n) { return about(n, s); }).sort(function (a, b) {
      return (a.state === 'done') - (b.state === 'done') || String(b.when || '').localeCompare(String(a.when || '')); });
  }
  function open(notes, s) { return pick(notes, s).filter(function (n) { return n.state !== 'done'; }).length; }
  // what the page knows and the file does not yet: a note just sent (until a reading lists it), a note not sent
  function merged(read, sent, unsent) {
    var have = {}; (read || []).forEach(function (n) { have[n.id] = true; });
    return (read || []).concat((sent || []).filter(function (n) { return !have[n.id]; }), unsent || []);
  }
  // how long ago, in words, and what a worker's last visual is to it (src_visuals.py's `how`)
  function when(sec) { return sec < 90 ? 'just now' : sec < 3600 ? Math.round(sec / 60) + ' min ago' : sec < 172800 ? Math.round(sec / 3600) + ' h ago' : Math.round(sec / 86400) + ' days ago'; }
  var HOW = { read: 'Looked at', shell: 'Named in a command', capture: "The branch's newest capture," };
  function caption(v, now) { return (HOW[v.how] || 'Last seen') + ' ' + when(Math.max(0, now - (v.at || 0))) + ' · ' + (v.name || ''); }
  // whether the panel shows a way to close it: a drawer always; docked, only what the owner opened himself
  function closable(s, docked, resting) { return !docked || !(s.follow || resting); }
  // what a note sends to the listener (notes.py, /note): its text, what it is about, and the stamp of the Then line he saw
  function sends(n) { return { text: n.text, kind: n.kind, about: n.about, title: n.title, lane: n.lane, asset: n.asset, page: n.page, then: n.then || '' }; }
  var pure = { slug: slug, short: short, about: about, pick: pick, open: open, merged: merged, when: when, caption: caption, closable: closable, sends: sends };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }

  var ROOT = document.body.dataset.root || '', LS = 'tw3d-notes-unsent';
  var sent = [], unsent = [], alive = null, subject = null, onclose = null, panel = null;
  try { unsent = JSON.parse(localStorage.getItem(LS) || '[]'); } catch (e) { unsent = []; }
  function keep() { try { localStorage.setItem(LS, JSON.stringify(unsent)); } catch (e) { /* a private window: kept until the page closes */ } }
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function all() { return merged(window.NOTES, sent, unsent); }
  function box() { return window.NOTEBOX && window.NOTEBOX.url ? window.NOTEBOX : null; }
  function post(path, body) {
    var b = box(); if (!b) return Promise.reject(new Error('no box'));
    body.key = b.key;
    // sent as plain text so the browser asks no question first; the listener reads it as JSON all the same
    return fetch(b.url + path, { method: 'POST', headers: { 'Content-Type': 'text/plain' }, body: JSON.stringify(body) })
      .then(function (r) { return r.json(); }).then(function (d) { if (!d.ok) throw new Error(d.why || 'refused'); alive = true; return d.note; });
  }
  function ping() {
    var b = box(); if (!b) { alive = false; return Promise.resolve(false); }
    return fetch(b.url + '/ping').then(function (r) { return r.json(); }).then(function (d) { alive = !!d.ok; return alive; }, function () { alive = false; return false; });
  }
  // send what waits in this browser, oldest first; what the listener refuses outright is kept, with its reason
  function flush() {
    if (!unsent.length) return Promise.resolve();
    var n = unsent[0];
    return post('/note', sends(n)).then(function (note) {
      unsent.shift(); keep(); sent.push(note); changed(); return flush();
    }, function (e) { if (alive) { n.why = e.message; keep(); } changed(); });
  }
  // s.then: the stamp of the "Then: ..." line the Decide page showed under the option he clicked (decide.js). It is kept
  // with a note that waits in this browser too, so a click sent hours later still says what he saw when he clicked.
  function write(s, text) {
    var n = { id: 'unsent-' + Date.now(), when: new Date().toISOString().slice(0, 16).replace('T', ' '), from: 'owner', state: 'unsent', text: text,
      kind: s.kind || 'page', about: s.id || '', title: s.title || '', lane: s.lane || '', asset: s.asset || '', page: location.pathname.split('/').slice(-1)[0] + location.search, then: s.then || '', answers: [] };
    unsent.push(n); keep(); changed();
    return flush();
  }
  function close(n) { return post('/close', { id: n.id }).then(function (note) { sent = sent.filter(function (x) { return x.id !== note.id; }); sent.push(note);
    (window.NOTES || []).forEach(function (x) { if (x.id === note.id) { x.state = 'done'; x.answers = note.answers; } }); changed(); }); }

  // ---- one note, and a list of them with the box under it
  function noteEl(n) {
    var d = el('div', 'b-note b-' + n.state), h = el('div', 'b-note-head');
    h.appendChild(el('span', 'b-state', n.state === 'done' ? 'answered' : n.state === 'unsent' ? 'not sent' : 'open'));
    h.appendChild(el('span', 'b-when', String(n.when || '').slice(0, 16)));
    var on = n.asset ? n.asset : n.lane ? short(n.lane) : n.title || '';
    if (on && subject && subject.kind === 'page') h.appendChild(el('span', 'b-on', on));
    d.appendChild(h); d.appendChild(el('p', 'b-text', n.text));
    (n.answers || []).forEach(function (a) { var r = el('p', 'b-answer'); r.appendChild(el('b', null, (a.by || 'an agent') + ' · ' + a.when)); r.appendChild(document.createTextNode(' ' + a.text)); d.appendChild(r); });
    if (n.state === 'unsent') {
      d.appendChild(el('p', 'b-why', n.why ? 'Refused: ' + n.why : 'Kept in this browser. It is sent when the note box answers.'));
      var rm = el('button', 'b-link', 'Discard'); rm.type = 'button'; rm.addEventListener('click', function () { unsent = unsent.filter(function (x) { return x !== n; }); keep(); changed(); }); d.appendChild(rm);
    } else if (n.state !== 'done' && alive) {
      var c = el('button', 'b-link', 'Close it'); c.type = 'button'; c.title = 'Mark this note as dealt with';
      c.addEventListener('click', function () { c.disabled = true; close(n).catch(function () { c.disabled = false; }); }); d.appendChild(c);
    }
    return d;
  }
  // The list is made again whenever the notes are read; the box under it is made once for a thing and then left
  // alone, so a reading does not move the caret or undo a resize while the owner types. What he was writing about a
  // thing is kept (drafts) while the panel is about something else, and is there again when it comes back.
  var drafts = {};
  function notesBlock(s, into) {
    var key = s.kind + '|' + s.id, form = into.querySelector(':scope > .b-form'), old = into.querySelector(':scope > .b-notes');
    if (into.dataset.about !== key) { into.innerHTML = ''; into.dataset.about = key; form = old = null; }
    var list = pick(all(), s), wrap = el('div', 'b-notes');
    wrap.appendChild(el('h4', null, list.length ? 'Notes · ' + list.filter(function (n) { return n.state !== 'done'; }).length + ' open' : 'Notes'));
    if (!list.length) wrap.appendChild(el('p', 'b-none', s.kind === 'page' ? 'No notes yet. Click anything on the board to leave one.' : 'None yet. What you write here, the agents on this read before they start.'));
    list.forEach(function (n) { wrap.appendChild(noteEl(n)); });
    if (old) into.replaceChild(wrap, old); else into.insertBefore(wrap, into.firstChild);
    if (s.kind === 'page' && s.id === 'all') return;
    if (!form) {
      form = el('form', 'b-form'); var ta = el('textarea'); ta.rows = 3; ta.maxLength = 4000; ta.value = drafts[key] || '';
      ta.placeholder = s.asset ? 'A note about the ' + (s.title || s.asset) + ' for the agents…' : 'A note for the agents on this…'; ta.setAttribute('aria-label', 'A new note');
      var row = el('div', 'b-form-row'), go = el('button', 'b-send', 'Leave note'); go.type = 'submit';
      row.appendChild(go); row.appendChild(el('span', 'b-say')); form.appendChild(ta); form.appendChild(row);
      form.addEventListener('submit', function (e) { e.preventDefault(); var t = ta.value.trim(); if (!t) { ta.focus(); return; } ta.value = ''; delete drafts[key]; write(s, t); });
      ta.addEventListener('input', function () { drafts[key] = ta.value; });
      ta.addEventListener('keydown', function (e) { if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) form.requestSubmit(); });
      into.appendChild(form);
    }
    form.querySelector('.b-say').textContent = alive === false ? 'Not connected: your note is kept in this browser and sent when the note box runs again (ops.py --watch).' : '';
  }
  // whether the owner is writing in the panel: the page then leaves the panel on what it is about
  function busy() { var ta = panel && panel.querySelector('.b-notes-here textarea'); return !!ta && (document.activeElement === ta || !!ta.value.trim()); }

  // ---- the panel: a drawer over the page, or (docked) a part of the page that is always there
  var docked = false, rest = null, resting = false, ALL = { kind: 'page', id: 'all', kindLabel: 'board', title: 'All notes', sub: 'What you wrote on the board, and what the agents answered.' };
  // dockEl: the page's place for the workers. While the drawer is open over it, `kept` is what that place shows.
  var dockEl = null, drawer = null, kept = null, NOBODY = { kind: 'page', id: 'nobody', kindLabel: 'agents', title: 'Nobody is selected', sub: 'Click a frog in the house: what it is doing, the last picture it had in its hands, and your notes to it.', bare: true };
  function forDock(s) { return !!s && (!!s.worker || s === NOBODY); }
  // do something to the workers' place while the drawer is open over it
  function inDock(f) {
    var was = [panel, subject, onclose, docked, resting];
    panel = dockEl; docked = true; subject = kept.s; onclose = kept.onclose; resting = kept.resting;
    try { f(); } finally { kept = { s: subject, onclose: onclose, resting: resting }; panel = was[0]; subject = was[1]; onclose = was[2]; docked = was[3]; resting = was[4]; }
  }
  var still = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  function keys() { document.addEventListener('keydown', function (e) { if (e.key === 'Escape' && !panel.hidden && !busy()) shut(); }); }
  function build() {
    panel = el('aside', 'b-panel'); panel.hidden = true; panel.setAttribute('aria-label', 'About what you clicked');
    document.body.appendChild(panel); keys();
  }
  // the page gives the panel a place of its own. `back`, when the owner closes what he opened, shows what the panel
  // is for on this page (the house: the worker it follows); when it shows nothing, the panel has every note
  function dock(into, back) {
    panel = dockEl = into; docked = true; rest = back || null; panel.classList.add('b-panel', 'b-docked'); panel.hidden = false; keys();
    if (subject && !forDock(subject)) subject = null;
    if (subject) draw(); else home();
  }
  function home() { if (rest) rest(); if (!subject) { show(NOBODY); resting = true; draw(); } }
  // a part of the panel is made again only when what it says changed: a film in it plays on through a reading
  function part(cls, sig, make) {
    var p = panel.querySelector(':scope > .' + cls);
    if (!p) { p = el('div', cls); panel.appendChild(p); }
    sig = JSON.stringify(sig);
    if (p.dataset.sig !== sig) { p.dataset.sig = sig; p.innerHTML = ''; make(p); }
    p.hidden = !p.firstChild;
    return p;
  }
  function draw() {
    var s = subject, C = root.Crew, w = docked && C ? s.worker : null, v = s.visual;
    if (panel.dataset.id !== s.kind + '|' + s.id) {          // someone else: the parts come in again (control.css)
      panel.dataset.id = s.kind + '|' + s.id; panel.scrollTop = 0;
      panel.classList.remove('b-swap'); void panel.offsetWidth; panel.classList.add('b-swap');
    }
    part('b-face', w ? [s.id, w.kind, w.state] : null, function (p) { if (w) p.appendChild(C.booth(w)); });
    part('b-head', [s.kind, s.id, s.title, s.sub, s.kindLabel, closable(s, docked, resting), !!s.follow], function (p) {
      var head = el('header'), t = el('div');
      t.appendChild(el('span', 'b-kind b-kind-' + s.kind, s.kindLabel || s.kind)); t.appendChild(el('h3', null, s.title || s.id || '')); if (s.sub) t.appendChild(el('p', 'b-sub', s.sub));
      head.appendChild(t);
      if (closable(s, docked, resting)) { var x = el('button', 'b-x', '×'); x.type = 'button'; x.title = docked ? 'Let go (Esc)' : 'Close (Esc)'; x.setAttribute('aria-label', x.title); x.addEventListener('click', shut); head.appendChild(x); }
      else if (s.follow) { var fl = el('span', 'b-follow', 'following'); fl.title = 'The profile follows whoever is at work. Click a frog to keep one here.'; head.appendChild(fl); }
      if (docked && s.worker) head.appendChild(el('p', 'b-hint', s.follow ? 'The house follows whoever is at work. Click a frog to keep one here.' : 'Kept here until you let go.'));
      p.appendChild(head);
    });
    part('b-facts', s.facts || [], function (p) { (s.facts || []).forEach(function (f) { p.appendChild(el('span', null, f)); }); });
    // what it is, in a few lines (a row of the queue: src_queue.py details)
    part('b-detail', s.detail || [], function (p) { (s.detail || []).forEach(function (t) { p.appendChild(el('p', t.indexOf('· ') === 0 ? 'b-point' : null, t)); }); });
    // the pictures that show it, each a click away at full size; a row that could show something and has nothing says so
    part('b-shots', [s.shots || [], !!s.detail], function (p) {
      if (!s.shots || !s.shots.length) { if (s.detail && s.detail.length && s.wantsShots) p.appendChild(el('p', 'b-novisual', 'No picture of this yet.')); return; }
      s.shots.forEach(function (x) { var a = el('a', 'b-shot'), m = el('img'); a.href = ROOT + x.src; a.target = '_blank'; a.rel = 'noopener'; a.title = 'Open ' + (x.name || 'it') + ' full size';
        m.loading = 'lazy'; m.alt = x.caption || x.name || ''; m.src = ROOT + x.src; a.appendChild(m); var f = el('figure'); f.appendChild(a); f.appendChild(el('figcaption', null, x.caption || x.name || '')); p.appendChild(f); });
    });
    // what he can say should happen: a click leaves that as a note of his, and the button shows it was said
    part('b-actions', [s.actions || [], pick(all(), s).filter(function (n) { return n.state !== 'done'; }).map(function (n) { return n.text; })], function (p) {
      if (!s.actions || !s.actions.length) return;
      p.appendChild(el('h4', null, 'What should happen?'));
      var said = pick(all(), s).filter(function (n) { return n.state !== 'done'; }).map(function (n) { return n.text; }), row = el('div', 'b-acts');
      s.actions.forEach(function (a) { var b = el('button', 'b-act' + (said.indexOf(a.say) >= 0 ? ' on' : ''), a.label); b.type = 'button'; b.title = 'Leaves the note: ' + a.say;
        b.setAttribute('aria-pressed', said.indexOf(a.say) >= 0 ? 'true' : 'false'); b.addEventListener('click', function () { if (said.indexOf(a.say) < 0) write(s, a.say); }); row.appendChild(b); });
      p.appendChild(row); p.appendChild(el('p', 'b-acts-say', 'A click leaves that as your note. Or say it in your own words below.'));
    });
    part('b-visual', [s.kind, s.id, v ? [v.src, v.at] : !!w], function (p) {
      if (!v) { if (w && (w.kind === 'session' || w.kind === 'agent')) p.appendChild(el('p', 'b-novisual', 'No picture or film in its hands yet.')); return; }
      var a = el('a', 'b-shot'), m, src = ROOT + v.src + '?' + v.at; a.href = ROOT + v.src; a.target = '_blank'; a.rel = 'noopener'; a.title = 'Open ' + v.name;
      if (v.kind === 'film') { m = el('video'); m.muted = true; m.loop = true; m.playsInline = true; m.setAttribute('playsinline', ''); m.preload = 'metadata'; m.src = src; if (still) m.controls = true; else m.autoplay = true; }
      else { m = el('img'); m.alt = 'The last picture ' + (s.title || 'it') + ' had in its hands'; m.src = src;
        m.addEventListener('load', function () { a.classList.toggle('b-tall', m.naturalHeight > m.naturalWidth * 1.1); }); }      // a tall one (a page) shows its top
      a.appendChild(m); var streak = el('i', 'b-streak'); streak.setAttribute('aria-hidden', 'true'); a.appendChild(streak);
      p.appendChild(a); p.appendChild(el('p', 'b-shot-cap'));
    });
    var cap = panel.querySelector(':scope > .b-visual .b-shot-cap'); if (cap && v) cap.textContent = caption(v, Date.now() / 1000);
    part('b-links', s.links || [], function (p) {
      (s.links || []).forEach(function (l) { var a = el('a', null, l.label + ' →'); a.href = l.href.indexOf(':') > 0 || l.href.charAt(0) === '#' ? l.href : ROOT + l.href; if (l.title) a.title = l.title; p.appendChild(a); });
    });
    part('b-models', (s.assets || []).map(function (a) { return [a.id, a.pic, a.status]; }), function (p) {
      if (!s.assets || !s.assets.length) return;
      p.appendChild(el('h4', null, s.assets.length === 1 ? 'The model it touches' : 'The ' + s.assets.length + ' models it touches'));
      var wall = el('div', 'b-wall');
      s.assets.slice(0, 12).forEach(function (a) { var c = el('a', 'b-asset'); c.href = ROOT + 'a/' + a.id + '.html'; c.title = a.name + (a.status ? ' · ' + a.status.toLowerCase().replace(/_/g, ' ') : '');
        if (a.pic) { var i = el('img'); i.loading = 'lazy'; i.alt = ''; i.src = ROOT + a.pic; c.appendChild(i); } c.appendChild(el('span', null, a.name || a.id)); wall.appendChild(c); });
      p.appendChild(wall);
    });
    var notes = part('b-notes-here', [s.kind, s.id], function () {}); notes.hidden = !!s.bare;
    if (!s.bare) notesBlock(s, notes);
  }
  function show(s, opt) {
    if (dockEl && !docked && forDock(s)) { inDock(function () { show(s, opt); }); return; }          // the house moved on under the drawer
    if (dockEl && docked && !forDock(s)) {          // not a worker: the drawer, and the workers' place keeps what it shows
      kept = { s: subject, onclose: onclose, resting: resting };
      if (!drawer) { drawer = el('aside', 'b-panel'); drawer.hidden = true; drawer.setAttribute('aria-label', 'About what you clicked'); document.body.appendChild(drawer); }
      panel = drawer; docked = false; subject = null; onclose = null; resting = false;
    }
    if (!panel) build();
    if (onclose && subject && (!s || s.id !== subject.id)) { var was = onclose; onclose = null; was(); }
    subject = s; onclose = (opt && opt.onclose) || null; resting = false;
    panel.hidden = false; if (!docked) document.body.classList.add('b-open'); draw();
    if (alive === null) ping().then(function () { if (subject) draw(); flush(); });
  }
  function shut() {
    if (!panel || panel.hidden || (docked && subject && !closable(subject, docked, resting))) return;
    if (!docked) { panel.hidden = true; document.body.classList.remove('b-open'); }
    subject = null; var was = onclose; onclose = null; if (was) was();
    if (dockEl && !docked) {          // the drawer closed: back to the workers' place, as it was left
      panel = dockEl; docked = true; subject = kept.s; onclose = kept.onclose; resting = kept.resting; kept = null;
      if (subject) draw(); else home();
      return;
    }
    if (docked && !subject) home();
  }

  // ---- on the page: a button that opens the panel and shows how many notes are open, blocks in the page, the cards
  var buttons = [], inline = [], listeners = [];
  function button(s, label) {
    var b = el('button', 'b-btn'); b.type = 'button'; b.title = 'Notes about ' + (s.title || s.id) + ', and where it leads';
    b.addEventListener('click', function (e) { e.preventDefault(); e.stopPropagation(); show(s); });
    buttons.push({ b: b, s: s, label: label || '' }); mark(buttons[buttons.length - 1]); return b;
  }
  function mark(x) { var n = open(all(), x.s); x.b.textContent = (x.label ? x.label + ' ' : '') + (n ? '✎ ' + n : '✎'); x.b.classList.toggle('b-has', n > 0); }
  function changed() {
    buttons = buttons.filter(function (x) { return x.b.isConnected || !x.seen; }); buttons.forEach(function (x) { x.seen = x.seen || x.b.isConnected; mark(x); });
    inline.forEach(function (x) { notesBlock(x.s, x.into); });
    if (subject && panel && !panel.hidden) draw();
    if (dockEl && !docked && kept && kept.s) inDock(draw);
    cards(); top();
    listeners.forEach(function (f) { f(); });
  }
  // every card of a model on the overview says how many notes are open on it
  function cards() {
    document.querySelectorAll('a.card[href^="a/"]').forEach(function (c) {
      var id = c.getAttribute('href').slice(2).replace(/\.html$/, ''), n = open(all(), { asset: id }), tag = c.querySelector('.b-count');
      if (!n) { if (tag) tag.remove(); return; }
      if (!tag) { tag = el('span', 'b-count'); (c.querySelector('.pic') || c).appendChild(tag); }
      tag.textContent = '✎ ' + n; tag.title = n + (n === 1 ? ' open note' : ' open notes');
    });
  }
  // the top bar: every note there is, one click away
  var topBtn = null;
  function top() {
    var bar = document.querySelector('header.top'); if (!bar) return;
    if (!topBtn) { topBtn = el('button', 'b-top'); topBtn.type = 'button'; topBtn.addEventListener('click', function () { show(ALL); if (docked) panel.scrollIntoView({ block: 'nearest', behavior: still ? 'auto' : 'smooth' }); });
      bar.insertBefore(topBtn, document.getElementById('k-now')); }
    var n = open(all(), { kind: 'page', id: 'all' });
    topBtn.textContent = n ? 'Notes ' + n : 'Notes'; topBtn.classList.toggle('b-has', n > 0);
  }
  document.querySelectorAll('[data-board]').forEach(function (into) {
    var d = into.dataset, s = { kind: d.board, id: d.id, title: d.title || d.id, lane: d.lane || '', asset: d.board === 'asset' ? d.id : '' };
    inline.push({ s: s, into: into });
  });
  // read the notes again every 20 seconds: an answer shows without a reload
  setInterval(function () {
    var s = document.createElement('script'); s.src = ROOT + 'data/notes.js?t=' + Date.now();
    s.onload = s.onerror = function () { s.remove(); var have = {}; (window.NOTES || []).forEach(function (n) { have[n.id] = true; }); sent = sent.filter(function (n) { return !have[n.id] || n.state === 'done'; }); changed(); };
    document.body.appendChild(s);
    ping().then(function (ok) { if (ok) flush(); });
  }, 20000);
  ping().then(function () { changed(); flush(); });
  changed();
  // note(subject, text) leaves a note without the panel (an option picked on the decisions page); notes(subject) are the ones about a thing
  root.Board = { open: show, close: shut, dock: dock, docked: function () { return docked; }, busy: busy, all: ALL, button: button, note: write, notes: function (s) { return pick(all(), s); }, count: function (s) { return open(all(), s); }, slug: slug, short: short,
    onchange: function (f) { listeners.push(f); }, pure: pure };
})(typeof window !== 'undefined' ? window : this);
