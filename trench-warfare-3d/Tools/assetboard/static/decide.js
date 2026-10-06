// The decisions page (decide.html): every decision that waits on the owner as a brief he can decide from: what it is
// for, the options as buttons (the first is the one the writer would take, and why), and the pictures or films that
// bear on it. briefs.py writes a brief and ops.py puts the open ones in data/briefs.js on every read. A click on an
// option leaves a note (board.js, notes.py): that is the owner's word, and the session that takes it up writes the
// row in decisions.md and closes the brief. Under the briefs: the questions of the queue nobody wrote a brief for
// yet, with what decisions.md says of each. Which brief is for which question is a plain function
// (test_assetboard.py runs it under node); the overview uses it to send a row of "Decide" to its brief.
(function (root) {
  'use strict';
  function slug(s) { return String(s || '').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 48); }
  // the open brief for a question of the queue: the one about that title (or called it), however it is spelled
  function match(briefs, title) {
    var want = slug(title); if (!want) return null;
    return (briefs || []).filter(function (b) { return b.state !== 'answered' && (slug(b.about) === want || slug(b.title) === want); })[0] || null;
  }
  // the briefs in the order the page shows them: the open ones oldest first, then the answered, newest first
  function order(briefs) {
    var open = (briefs || []).filter(function (b) { return b.state !== 'answered'; }).sort(function (a, b) { return String(a.asked).localeCompare(String(b.asked)); });
    var done = (briefs || []).filter(function (b) { return b.state === 'answered'; }).sort(function (a, b) { return String((b.answer || {}).when).localeCompare(String((a.answer || {}).when)); });
    return { open: open, done: done };
  }
  // the questions of the queue with no open brief
  function bare(briefs, questions) { return (questions || []).filter(function (q) { return !match(briefs, q.title); }); }
  // what a click on an option says, as the note that is left
  function word(b, key, said) {
    var o = (b.options || []).filter(function (x) { return x.key === key; })[0];
    return o ? key + ': ' + o.text + (said ? '\n' + said : '') : String(said || '');
  }
  var pure = { slug: slug, match: match, order: order, bare: bare, word: word };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }
  root.Briefs = pure;

  var page = document.getElementById('decide'), C = root.Crew, B = root.Board;
  if (!page || !C) return;
  var el = C.el, ROOT = document.body.dataset.root || '', still = root.matchMedia && root.matchMedia('(prefers-reduced-motion: reduce)').matches;
  function subject(b) { return { kind: 'page', id: 'brief:' + b.id, kindLabel: 'decision', title: b.title, lane: b.lane || '' }; }
  // what the owner already said about a brief on this page: his last note about it
  function said(b) { var n = B && B.notes ? B.notes(subject(b)).filter(function (x) { return x.about === 'brief:' + b.id; }) : []; return n.sort(function (x, y) { return String(y.when).localeCompare(String(x.when)); })[0] || null; }

  function evidence(b) {
    var box = el('div', 'd-evidence d-n' + Math.min(3, b.evidence.length));
    b.evidence.forEach(function (e) {
      var f = el('figure'), a = el('a'), m; a.href = ROOT + e.src; a.target = '_blank'; a.rel = 'noopener'; a.title = 'Open it full size';
      if (!e.src) { f.appendChild(el('p', 'd-gone', 'This piece of evidence is gone: ' + e.file)); }
      else if (e.kind === 'film') { m = el('video'); m.muted = true; m.loop = true; m.playsInline = true; m.controls = true; m.preload = 'metadata'; m.src = ROOT + e.src; if (!still) m.autoplay = true; f.appendChild(m); }
      else { m = el('img'); m.loading = 'lazy'; m.alt = e.caption; m.src = ROOT + e.src; a.appendChild(m); f.appendChild(a); }
      f.appendChild(el('figcaption', null, e.caption)); box.appendChild(f);
    });
    if (!b.evidence.length) box.appendChild(el('p', 'd-none', 'Nothing to show: ' + String(b.no_evidence || 'no reason given').replace(/\.+$/, '') + '.'));
    return box;
  }
  function card(b) {
    var c = el('article', 'd-card' + (b.state === 'answered' ? ' d-done' : '')), tx = el('div', 'd-text'), mine = said(b); c.id = b.id;
    tx.appendChild(el('p', 'd-meta', 'asked ' + String(b.asked).slice(0, 10) + (b.lane ? ' · ' + b.lane.replace(/^lane\/(show|sim)\//, '') : '') + (b.about && b.about !== b.title ? ' · in the queue as "' + b.about + '"' : '')));
    tx.appendChild(el('h3', null, b.title)); tx.appendChild(el('p', 'd-for', b.what_for));
    var ops = el('div', 'd-options'); ops.setAttribute('role', 'group'); ops.setAttribute('aria-label', 'The options');
    var took = b.answer ? b.answer.option : mine && /^[A-D]: /.test(mine.text) ? mine.text.charAt(0) : '';
    b.options.forEach(function (o) {
      var bt = el('button', 'd-opt' + (o.key === b.pick ? ' d-pick' : '') + (o.key === took ? ' on' : '')); bt.type = 'button'; bt.disabled = b.state === 'answered'; bt.setAttribute('aria-pressed', o.key === took ? 'true' : 'false');
      bt.appendChild(el('span', 'd-key', o.key)); bt.appendChild(el('span', 'd-opt-text', o.text)); if (o.key === b.pick) bt.appendChild(el('span', 'd-tag', 'the writer would'));
      bt.addEventListener('click', function () { if (B && B.note) B.note(subject(b), pure.word(b, o.key, '')); });
      ops.appendChild(bt);
    });
    tx.appendChild(ops);
    var why = el('p', 'd-why'); why.appendChild(el('b', null, 'Why ' + b.pick + ': ')); why.appendChild(document.createTextNode(b.why)); tx.appendChild(why);
    if (b.state !== 'answered') {
      var form = el('form', 'd-say'), inp = el('input'), go = el('button', null, 'Say it'); inp.type = 'text'; inp.maxLength = 600; inp.placeholder = 'Or in your own words…'; inp.setAttribute('aria-label', 'Your own answer to: ' + b.title); go.type = 'submit';
      form.appendChild(inp); form.appendChild(go);
      form.addEventListener('submit', function (e) { e.preventDefault(); var t = inp.value.trim(); if (!t) { inp.focus(); return; } inp.value = ''; if (B && B.note) B.note(subject(b), t); });
      tx.appendChild(form);
    }
    if (b.answer) tx.appendChild(el('p', 'd-said d-closed', 'Decided ' + b.answer.when + ': ' + [b.answer.option === 'other' ? '' : b.answer.option, b.answer.said ? '"' + b.answer.said + '"' : ''].filter(Boolean).join(' · ')));
    else if (mine) tx.appendChild(el('p', 'd-said' + (mine.state === 'unsent' ? ' d-unsent' : ''), (mine.state === 'unsent' ? 'Not sent yet (the note box is not running): ' : 'You said, ' + String(mine.when).slice(5, 16) + ': ') + mine.text.split('\n').join(' · ') +
      (mine.state === 'done' ? ' · taken up' : mine.state === 'unsent' ? '' : ' · waits for a session to take it up')));
    c.appendChild(tx); c.appendChild(evidence(b));
    return c;
  }
  // thirty briefs are a long page: every open one by its title at the top, a click away, marked once he has said something
  function index(open) {
    var ix = document.getElementById('d-index'); if (!ix) return;
    var fold = document.getElementById('d-fold');
    if (fold) { fold.hidden = open.length < 4; if (!index.set) { index.set = true; fold.open = !(root.matchMedia && root.matchMedia('(max-width: 760px)').matches); } }          // on a phone thirty titles are two screens: folded until asked for
    ix.innerHTML = '';
    open.forEach(function (b) {
      var a = el('a', said(b) ? 'said' : null, b.title), n = b.evidence.length, films = b.evidence.filter(function (e) { return e.kind === 'film'; }).length;
      a.href = '#' + b.id;
      if (n) a.appendChild(el('span', null, films ? (n - films ? n - films + ' + ' : '') + (films === 1 ? 'film' : films + ' films') : n === 1 ? '1 picture' : n + ' pictures'));
      ix.appendChild(a);
    });
  }
  function draw() {
    var all = pure.order(root.BRIEFS || []), qs = (root.QUEUE && root.QUEUE.decide) || [], left = pure.bare(root.BRIEFS, qs);
    if (root.OPS) C.nowPill(root.OPS);
    var n = document.getElementById('d-count'); if (n) n.textContent = all.open.length + (all.open.length === 1 ? ' brief open' : ' briefs open') + (left.length ? ' · ' + left.length + ' questions without one' : '');
    var box = document.getElementById('d-open'), inp = box.contains(document.activeElement) && document.activeElement.tagName === 'INPUT' ? document.activeElement : null;
    var sig = JSON.stringify([all.open, all.open.map(said)]);
    if (box.dataset.sig !== sig && !(inp && inp.value)) {          // not while he is typing an answer of his own
      box.dataset.sig = sig; box.innerHTML = '';
      all.open.forEach(function (b) { box.appendChild(card(b)); });
      index(all.open);
      if (!all.open.length) box.appendChild(el('p', 'd-empty', 'No brief is open.' + (left.length ? ' The questions below have none yet.' : ' Nothing waits on a decision.')));
    }
    var bare = document.getElementById('d-bare');
    if (bare.dataset.sig !== JSON.stringify(left)) {
      bare.dataset.sig = JSON.stringify(left); bare.innerHTML = ''; bare.hidden = !left.length;
      if (left.length) bare.appendChild(el('h3', 'd-h', 'In the queue, no brief yet'));
      left.forEach(function (q) {
        var r = el('div', 'd-row'); r.id = 'q-' + slug(q.title);
        var t = el('div'); t.appendChild(el('b', null, q.title)); t.appendChild(el('span', 'd-age', (q.days != null ? q.days + ' d' : '') + (q.choice ? ' · the agent chose, yours to overrule' : '')));
        t.appendChild(el('p', null, q.text || ''));
        r.appendChild(t); if (B) r.appendChild(B.button({ kind: 'queue', id: 'decide: ' + q.title, kindLabel: 'decide', title: q.title, sub: q.text || '', facts: [] }, 'Answer'));
        bare.appendChild(r);
      });
    }
    // a question he answered that its lane has written down and not landed: decided, and said to be where it is
    var done = document.getElementById('d-done'), ans = ((root.QUEUE && root.QUEUE.answered) || []).filter(function (q) {
      return !all.done.some(function (b) { return slug(b.about) === slug(q.title) || slug(b.title) === slug(q.title); });          // its closed brief says it already
    });
    if (done.dataset.sig !== JSON.stringify([all.done, ans])) {
      done.dataset.sig = JSON.stringify([all.done, ans]); done.innerHTML = ''; done.hidden = !all.done.length && !ans.length;
      if (all.done.length || ans.length) done.appendChild(el('h3', 'd-h', 'Decided this week'));
      ans.forEach(function (q) {
        var r = el('div', 'd-row'), t = el('div'); t.appendChild(el('b', null, q.title));
        t.appendChild(el('p', null, 'You answered this. It is written down on ' + String(q.lane || 'a lane').replace(/^lane\/(show|sim)\//, '') + ' and the other lanes see it once that lane lands.'));
        r.appendChild(t); done.appendChild(r);
      });
      all.done.forEach(function (b) { done.appendChild(card(b)); });
    }
    if (location.hash && !draw.jumped) { var at = document.getElementById(decodeURIComponent(location.hash.slice(1))); if (at) { draw.jumped = true; at.scrollIntoView(); at.classList.add('d-at'); } }
  }
  if (B && B.onchange) B.onchange(draw);
  C.live(draw);          // the briefs are read again with the floor (crew.js): a new one shows without a reload
})(typeof window !== 'undefined' ? window : this);
