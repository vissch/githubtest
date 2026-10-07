// The ideas on the control screen (index.html, #ideas): what could be done next, each a card with a picture, and the
// owner says which are done. ideas.py writes an idea and ops.py puts the open ones in data/ideas.js on every read.
// The card has three buttons: "Do it" is his yes to the route the card shows (the click carries the route's stamp, as
// a click on a brief carries the stamp of its Then line), "Not now" parks the idea, "Never" closes it for good; what
// he typed in the box goes with the two as his reason, and sent alone it asks the agent for a better version. Under
// the card: a button for three more ideas and a box to ask for ideas on something. Each leaves a note (board.js,
// notes.py); the watcher reads it on its next pass and starts the ideas agent (ideas.py tick), and the line under the
// box says what is going on. The ordering and the wording are plain functions (test_assetboard.py runs them under node).
(function (root) {
  'use strict';
  // the ideas in the order the page shows them: the open ones he has not answered, newest first; then the open ones
  // his answer is on its way for; `done` are the accepted, newest first (what he turned down is not shown again)
  function order(ideas, said) {
    var has = function (i) { return !!(said && said(i)); };
    var open = (ideas || []).filter(function (i) { return i.state === 'open'; }).sort(function (a, b) { return has(a) - has(b) || String(b.made).localeCompare(String(a.made)); });
    var done = (ideas || []).filter(function (i) { return i.state === 'accepted'; }).sort(function (a, b) { return String((b.answer || {}).when).localeCompare(String((a.answer || {}).when)); });
    return { open: open, done: done };
  }
  // the one shown large: the one he clicked while it is still open, else the first
  function featured(open, sel) { return (open || []).filter(function (i) { return i.id === sel; })[0] || (open || [])[0] || null; }
  // what a button says, as the note that is left: its own words, and his reason after a colon
  function word(says, reason) { reason = String(reason || '').trim(); return reason ? says + ': ' + reason : says; }
  // what a note of his about an idea asked for: one of the buttons' words, or '' for words of his own
  function asked(says, text) {
    text = String(text || '');
    return (says || []).filter(function (s) { return text === s || text.indexOf(s + ':') === 0 || text.indexOf(s + '\n') === 0; })[0] || '';
  }
  function clock(when) { return String(when || '').slice(11, 16); }
  // the line under the box: what the agent is doing about what he asked, in words
  function status(d, unsent) {
    d = d || {};
    if (unsent) return 'Not connected. It is asked as soon as the board is back.';
    if (d.running) return 'The ideas agent is at work since ' + clock(d.running.since) + (d.running.asked === 'auto' ? ', on one idea for this screen' : d.running.asked === 'button' ? ', on three more' : ', on: ' + d.running.asked) +
      '. A run takes a few minutes.' + ((d.asked || []).length ? ' ' + (d.asked.length === 1 ? '1 more request waits' : d.asked.length + ' more requests wait') + ' behind it.' : '');
    if ((d.asked || []).length) return d.left === 0 ? 'Asked. Today\'s runs are used up: it starts tomorrow.' : d.off ? 'Asked, but it could not start: ' + d.off + '.' : 'Asked. The agent starts within a minute.';
    if (d.last && !d.last.ideas) return 'The last run made no idea' + (d.last.why ? ' (' + d.last.why + ')' : '') + '.';
    return d.left === 0 ? 'Today\'s runs are used up; more tomorrow.' : '';
  }
  // where a reference was found, as the card says it: the site, without the rest of the link
  function site(source) { var m = /^https?:\/\/(?:www\.)?([^\/]+)/.exec(String(source || '')); return m ? m[1] : ''; }
  var SIZE = { S: 'about a day', M: 'a few days', L: 'a week or more' };
  // the master's score in words: "R1 C3 MAJOR" is risk 1 of 5, change 3 of 5, and a major decision
  function score(sc) { var m = /^R(\d) C(\d)(.*)$/.exec(String(sc || '')); return m ? 'risk ' + m[1] + ' · change ' + m[2] + (/MAJOR/.test(m[3]) ? ' · major' : '') : String(sc || ''); }
  function meta(i) { return [i.kind, SIZE[i.size] || i.size, score(i.score)].filter(Boolean).join(' · '); }
  var pure = { order: order, featured: featured, word: word, asked: asked, status: status, site: site, meta: meta, score: score };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }
  root.Ideas = pure;

  var home = document.getElementById('ideas-cards'), C = root.Crew, B = root.Board;
  if (!home || !C) return;
  var el = C.el, ROOT = document.body.dataset.root || '', still = root.matchMedia && root.matchMedia('(prefers-reduced-motion: reduce)').matches, sel = '';
  var MORE = { kind: 'page', id: 'ideas:more', title: 'Three more ideas' }, ASK = { kind: 'page', id: 'ideas:request', title: 'Ideas on request' };
  function subject(i) { return { kind: 'page', id: 'idea:' + i.id, title: i.title, then: i.stamp }; }
  function notes(s) { return B && B.notes ? B.notes(s).filter(function (n) { return n.about === s.id; }) : []; }
  // his last word about an idea that no reading has acted on yet
  function said(i) { return notes(subject(i)).filter(function (n) { return n.state !== 'done'; }).sort(function (x, y) { return String(y.when).localeCompare(String(x.when)); })[0] || null; }
  function waiting(s) { return notes(s).filter(function (n) { return n.state !== 'done'; }); }

  function pictures(i) {
    var box = el('div', 'd-evidence i-pictures d-n' + Math.min(3, i.pictures.length));
    i.pictures.forEach(function (p) {
      var f = el('figure'), a = el('a'), m; a.href = ROOT + p.src; a.target = '_blank'; a.rel = 'noopener'; a.title = 'Open it full size';
      if (!p.src) f.appendChild(el('p', 'd-gone', 'This picture is gone: ' + p.file));
      else if (p.film) { m = el('video'); m.muted = true; m.loop = true; m.playsInline = true; m.controls = true; m.preload = 'metadata'; m.src = ROOT + p.src; if (!still) m.autoplay = true; f.appendChild(m); }
      else { m = el('img'); m.loading = 'lazy'; m.alt = p.caption; m.src = ROOT + p.src; a.appendChild(m); f.appendChild(a); }
      // what kind of picture it is stands on the picture: a sketch is not game art, and he sees that at a glance
      f.appendChild(el('span', 'i-pkind i-p-' + p.kind, p.kind === 'reference' && pure.site(p.source) ? 'reference · ' + pure.site(p.source) : p.kind));
      f.appendChild(el('figcaption', null, p.caption)); box.appendChild(f);
    });
    return box;
  }
  function route(i) {
    var r = el('ol', 'i-route'); r.setAttribute('aria-label', 'Who works on it, in order');
    i.route.forEach(function (s) { var li = el('li', s.own ? 'i-own' : null, s.says); li.title = s.own ? 'A decision of yours, put to you with pictures' : 'The ' + s.role + ' agent'; r.appendChild(li); });
    return r;
  }
  function card(i, says) {
    var mine = said(i), did = mine ? pure.asked(says, mine.text) : '', c = el('article', 'd-card i-card' + (did ? ' d-yours' : '')), tx = el('div', 'd-text'); c.id = 'idea-' + i.id;
    tx.appendChild(el('p', 'd-meta', pure.meta(i)));
    tx.appendChild(el('h3', null, i.title)); tx.appendChild(el('p', 'd-for', i.pitch));
    var why = el('p', 'd-why'); why.appendChild(el('b', null, 'Why now: ')); why.appendChild(document.createTextNode(i.why_now)); tx.appendChild(why);
    tx.appendChild(el('p', 'i-h', 'What happens next, if you say yes'));
    tx.appendChild(route(i));
    var inp = el('input'); inp.type = 'text'; inp.maxLength = 600; inp.placeholder = 'A reason, or what to change…'; inp.setAttribute('aria-label', 'Your words about: ' + i.title);
    var acts = el('div', 'i-acts'); acts.setAttribute('role', 'group'); acts.setAttribute('aria-label', 'Your answer');
    says.forEach(function (s, n) {
      var bt = el('button', 'i-act' + (n ? '' : ' i-go') + (n === 2 ? ' i-never' : '') + (did === s ? ' on' : ''), s); bt.type = 'button'; bt.disabled = !!did; bt.setAttribute('aria-pressed', did === s ? 'true' : 'false');
      bt.title = n === 0 ? 'Yes: it starts down the route above' : n === 1 ? 'Parked: it may come back in two weeks' : 'Closed for good: it is not put to you again';
      bt.addEventListener('click', function () { if (B && B.note) { var t = inp.value; inp.value = ''; B.note(subject(i), pure.word(s, n ? t : '')); } });
      acts.appendChild(bt);
    });
    tx.appendChild(acts);
    if (!did) {
      var form = el('form', 'd-say'), go = el('button', null, 'Improve this one'); go.type = 'submit'; go.title = 'The agent makes a new version of this idea after your words';
      form.appendChild(inp); form.appendChild(go);
      form.addEventListener('submit', function (e) { e.preventDefault(); var t = inp.value.trim(); if (!t) { inp.focus(); return; } inp.value = ''; if (B && B.note) B.note(subject(i), t); });
      tx.appendChild(form);
      tx.appendChild(el('p', 'i-hint', 'Typed words go along with Not now or Never as your reason.'));
    }
    if (mine) tx.appendChild(el('p', 'd-said' + (mine.state === 'unsent' ? ' d-unsent' : ''), (mine.state === 'unsent' ? 'Not connected, sends when the board is back: ' : 'You said, ' + String(mine.when).slice(11, 16) + ': ') + mine.text.split('\n').join(' · ') +
      (mine.state === 'unsent' ? '' : did ? ' · saved' : ' · the agent is asked for a better version')));
    c.appendChild(tx); c.appendChild(pictures(i));
    return c;
  }
  function small(i, on) {
    var a = el('button', 'i-small' + (on ? ' on' : '')); a.type = 'button'; a.setAttribute('aria-pressed', on ? 'true' : 'false'); a.title = 'Show this idea large';
    var p = i.pictures.filter(function (x) { return x.src && !x.film; })[0];
    if (p) { var m = el('img'); m.loading = 'lazy'; m.alt = ''; m.src = ROOT + p.src; a.appendChild(m); }
    var t = el('span', 'i-small-text'); t.appendChild(el('b', null, i.title)); var mine = said(i); t.appendChild(el('span', null, i.kind + (!mine ? '' : pure.asked((root.IDEAS || {}).says, mine.text) ? ' · you answered' : ' · a better one is asked for'))); a.appendChild(t);
    // the large card changes above the row: bring it into view and put the keyboard on it
    a.addEventListener('click', function () { sel = i.id; home.dataset.sig = ''; draw(); var c = home.querySelector('.i-card h3'); if (c) { c.tabIndex = -1; c.focus({ preventScroll: true }); c.scrollIntoView({ block: 'nearest', behavior: still ? 'auto' : 'smooth' }); } });
    return a;
  }
  function askBar(d) {
    var bar = el('div', 'i-ask'), more = el('button', 'i-more', '3 more ideas'), form = el('form', 'd-say i-ask-form'), inp = el('input'), go = el('button', null, 'Ask');
    more.type = 'button'; more.disabled = waiting(MORE).length > 0 || d.left === 0; more.title = 'The ideas agent makes three more, each of another kind'; if (d.left === 0) more.textContent = 'More tomorrow';
    more.addEventListener('click', function () { if (B && B.note) B.note(MORE, 'Three more ideas.'); });
    inp.type = 'text'; inp.maxLength = 300; inp.placeholder = 'Ask for ideas about… (the frog faction, the HUD, how a match ends)'; inp.setAttribute('aria-label', 'Ask for ideas about something'); go.type = 'submit';
    form.appendChild(inp); form.appendChild(go);
    form.addEventListener('submit', function (e) { e.preventDefault(); var t = inp.value.trim(); if (!t) { inp.focus(); return; } inp.value = ''; if (B && B.note) B.note(ASK, t); });
    bar.appendChild(more); bar.appendChild(form);
    return bar;
  }
  function draw() {
    var d = root.IDEAS || { ideas: [], says: ['Do it', 'Not now', 'Never'] }, all = pure.order(d.ideas, said), says = d.says || ['Do it', 'Not now', 'Never'];
    var sec = document.getElementById('ideas'); if (sec) sec.hidden = false;
    var unsent = waiting(MORE).concat(waiting(ASK)).filter(function (n) { return n.state === 'unsent'; }).length > 0;
    var mine = waiting(MORE).concat(waiting(ASK)).map(function (n) { return n.id + n.state; });
    var at = document.activeElement, typing = home.contains(at) && at.tagName === 'INPUT' && at.value;
    var typed = {}; [].forEach.call(home.querySelectorAll('input[type=text]'), function (x) { if (x.value) typed[x.getAttribute('aria-label')] = x.value; });
    var sig = JSON.stringify([d, all.open.map(function (i) { var n = said(i); return n ? n.id + n.state : ''; }), mine, sel]);
    if (typing || home.dataset.sig === sig) return;          // not while he is typing: the box keeps his words
    home.dataset.sig = sig; home.innerHTML = '';
    var big = pure.featured(all.open, sel);
    if (big) home.appendChild(card(big, says));
    else home.appendChild(el('p', 'i-empty', d.running ? 'No idea is open. One is being made for this screen.' : 'No idea is open. Ask for some below.'));
    if (all.open.length > 1) {
      var row = el('div', 'i-smalls'); row.setAttribute('aria-label', 'The other open ideas');
      all.open.forEach(function (i) { row.appendChild(small(i, i === big)); });
      home.appendChild(row);
    }
    home.appendChild(askBar(d));
    var line = pure.status(d.asked && d.asked.length || !mine.length ? d : { asked: [{}], left: d.left, off: d.off, running: d.running }, unsent);
    [].forEach.call(home.querySelectorAll('input[type=text]'), function (x) { var v = typed[x.getAttribute('aria-label')]; if (v) x.value = v; });          // words typed and left are still there
    var stuck = !d.running && (d.off || d.left === 0) && ((d.asked || []).length || mine.length);
    var st = el('p', 'i-status' + (d.running ? ' i-running' : stuck ? ' i-off' : ''), line); st.setAttribute('aria-live', 'polite'); st.hidden = !line; home.appendChild(st);
    if (all.done.length) {
      var dn = el('div', 'i-done'); dn.appendChild(el('h3', 'd-h', 'Accepted, on their way'));
      all.done.forEach(function (i) {
        var r = el('div', 'd-row'), t = el('div'); t.appendChild(el('b', null, i.title)); t.appendChild(el('span', 'd-age', 'you said yes ' + String((i.answer || {}).when).slice(5, 16) + (i.routed ? ' · on the board as ' + i.routed : ' · queued for the pipeline')));
        t.appendChild(route(i)); r.appendChild(t); dn.appendChild(r);
      });
      home.appendChild(dn);
    }
    var n = document.getElementById('ideas-n'); if (n) n.textContent = all.open.length ? '· ' + all.open.length + ' to pick from' : '';
  }
  if (B && B.onchange) B.onchange(draw);
  C.live(draw);          // the ideas are read again with the floor (crew.js): a new one shows without a reload
})(typeof window !== 'undefined' ? window : this);
