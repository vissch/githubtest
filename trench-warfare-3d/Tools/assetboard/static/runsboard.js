// The Runs page drawn (runs.html): a card a run, the newest first. On each: when it was and how it ended, what was
// done and what is left (the run reader's report, runreport.py; until there is one, a line that says so and a button
// to have it read), what it leaves for the owner (a brief of its own is the Decide page's card, so he decides here
// with its pictures; what a leg asked that no brief holds is listed as the leg wrote it), and its units, each with
// the agent that worked it and, a click deeper, every leg's own words. The words are runs.js's. A strip over the
// cards has a bar a run; a click on one goes to its card. What is wrong with the reading stands above (#r-warn).
(function () {
  'use strict';
  var T = window.Runs, C = window.Crew, Bd = window.Board, Br = window.Briefs;
  var board = document.getElementById('r-board');
  if (!T || !board) return;
  var only = false, open = {};          // "only what needs you", and the folds he opened: both outlive a redraw
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function link(cls, text, href, title) { var a = el('a', cls, text); a.href = href; a.target = '_blank'; a.rel = 'noopener'; if (title) a.title = title; return a; }
  function repo() { return String((window.RUNS || {}).repo || ''); }
  function saidOf(r) {
    if (!Bd) return [];
    return Bd.notes(T.subject(r)).filter(function (n) { return n.state !== 'done'; }).map(function (n) { return n.text; });
  }
  // a fold that stays as he left it
  function fold(key, cls, summary, start) {
    var d = el('details', cls), s = el('summary'); s.appendChild(summary); d.appendChild(s);
    d.open = key in open ? open[key] : !!start;
    d.addEventListener('toggle', function () { open[key] = d.open; });
    return d;
  }
  function chips(list, cls) { var p = el('p', cls || 'r-chips'); list.forEach(function (c) { if (c) p.appendChild(el('span', 'k-qchip' + (c.cls ? ' ' + c.cls : ''), c.text || c)); }); return p; }
  function laneLink(lane) { return repo() ? link('r-link', T.short(lane), repo() + '/tree/' + lane, 'The lane on GitHub: ' + lane) : el('span', 'r-lane', T.short(lane)); }
  function commit(sha, subject) { var a = repo() ? link('r-link r-sha', sha, repo() + '/commit/' + sha, subject || 'The commit on GitHub') : el('span', 'r-sha', sha); return a; }

  // one leg: who, how long, what it said, what it changed and what it asked
  function legEl(run, u, l) {
    var w = T.leg(l), box = el('div', 'r-leg r-said-' + w.key), top = el('p', 'r-leg-top');
    top.appendChild(el('b', null, w.n)); w.facts.forEach(function (f) { top.appendChild(el('span', 'r-fact', f)); }); top.appendChild(el('span', 'r-verdict r-v-' + w.key, w.said));
    box.appendChild(top);
    if (l.result) box.appendChild(el('p', 'r-result', l.result));
    if (l.changed.length) { var ul = el('ul', 'r-changed'); l.changed.forEach(function (c) { ul.appendChild(el('li', null, c)); }); box.appendChild(ul); }
    if (l.needs) { var nd = el('p', 'r-needs'); nd.appendChild(el('b', null, 'Asked of you: ')); nd.appendChild(document.createTextNode(l.needs)); box.appendChild(nd); }
    if (l.next) { var nx = el('p', 'r-next'); nx.appendChild(el('b', null, 'Next: ')); nx.appendChild(document.createTextNode(l.next)); box.appendChild(nx); }
    var made = l.commits.length ? l.commits.map(function (c) { return [c.sha, c.subject]; }) : l.shas.map(function (s) { return [s, '']; });
    if (made.length) { var cm = el('p', 'r-commits', l.commits.length ? 'Commits: ' : 'Names: '); made.forEach(function (c) { cm.appendChild(commit(c[0], c[1])); if (c[1]) cm.appendChild(el('span', 'r-subject', c[1])); }); if (l.more) cm.appendChild(el('span', 'r-subject', 'and ' + l.more + ' more')); box.appendChild(cm); }
    var whole = fold(run.id + '|' + l.n + '|report', 'r-whole', el('span', null, 'Its report, as it wrote it'), false);
    whole.appendChild(el('pre', null, l.report + (l.cut ? '\n… (cut: the record holds more)' : ''))); box.appendChild(whole);
    return box;
  }
  // one unit: its verdict, what was done on it, the agent, and its legs a click deeper
  function unitEl(run, u) {
    var v = T.verdict(u, run), who = T.agents(u), sum = el('span', 'r-unit-sum'), words = el('span', 'r-unit-what');
    var line = run.report && run.report.units && run.report.units[u.id];
    sum.appendChild(el('span', 'r-verdict r-v-' + v.key, v.label));
    words.appendChild(el('span', 'r-unit-top', line || u.id));
    if (line) words.appendChild(el('span', 'r-unit-id', u.id));
    else if (u.goal) words.appendChild(el('span', 'r-unit-goal', u.goal));
    var meta = el('span', 'r-unit-meta');
    meta.appendChild(el('span', 'r-agent', who.role + (who.phases.length ? ': ' + who.phases.join(', then ') : '')));
    meta.appendChild(el('span', 'r-fact', T.n(u.legs.length, '1 leg', 'legs') + ' · ' + T.span(u.seconds) + ' · ' + T.money(u.usd)));
    words.appendChild(meta); sum.appendChild(words);
    var d = fold(run.id + '|' + u.id, 'r-unit r-u-' + v.key, sum, false), body = el('div', 'r-unit-body'), where = el('p', 'r-where');
    if (u.lane) { where.appendChild(el('span', null, 'Lane: ')); where.appendChild(laneLink(u.lane)); }
    if (u.head) { where.appendChild(el('span', null, ' · it stood at ')); where.appendChild(commit(u.head, 'The lane\'s head when the unit passed')); where.appendChild(el('span', null, ' when it passed')); }
    if (where.childNodes.length) body.appendChild(where);
    if (line && u.goal) { var g = el('p', 'r-goal'); g.appendChild(el('b', null, 'What it was asked: ')); g.appendChild(document.createTextNode(u.goal)); body.appendChild(g); }
    u.why.forEach(function (w) { var p = el('p', 'r-why'); p.appendChild(el('b', null, 'The runner said: ')); p.appendChild(document.createTextNode(w)); body.appendChild(p); });
    if (!u.legs.length) body.appendChild(el('p', 'r-none', 'No leg ran on it.'));
    u.legs.forEach(function (l) { body.appendChild(legEl(run, u, l)); });
    d.appendChild(body);
    return d;
  }
  // a brief of the run's: the Decide page's own card when the page has the brief whole, so he decides here; else in short
  function briefEl(b) {
    var full = (window.BRIEFS || []).filter(function (x) { return x.id === b.id; })[0], box = el('div', 'r-brief r-how-' + b.how);
    box.appendChild(el('p', 'r-how', T.how(b) + (b.leg ? ' (leg ' + ('0' + b.leg).slice(-2) + ')' : '')));
    if (full && Br && Br.card) { box.appendChild(Br.card(full)); return box; }
    var c = el('div', 'r-brief-short');
    c.appendChild(el('h4', null, b.title)); if (b.what_for) c.appendChild(el('p', 'r-for', b.what_for));
    c.appendChild(el('p', 'r-said', b.answer ? T.decided(b) : 'Open since ' + String(b.asked).slice(0, 10) + '.'));
    if (!b.answer && b.shown) { var go = el('a', 'r-go', 'Decide it'); go.href = 'decide.html#' + b.id; c.appendChild(go); }
    box.appendChild(c);
    return box;
  }
  function askEl(run, a) {
    var p = el('div', 'r-ask');
    p.appendChild(el('p', 'r-ask-who', (a.kind === 'blocked' ? 'A unit ended blocked' : 'A leg asked') + ' · leg ' + ('0' + a.leg).slice(-2) + ' · ' + a.unit));
    p.appendChild(el('p', 'r-ask-text', a.text));
    return p;
  }
  // what the run leaves for him
  function youEl(run, said) {
    var d = T.decisions(run), box = el('section', 'r-you'), k = d.open.length + d.asks.length, st = T.reading(run, window.RUNS, said);
    var h = el('h4', 'r-h'); h.appendChild(el('span', null, 'Needs you')); h.appendChild(el('span', 'r-n' + (k ? ' r-n-hot' : ''), String(k))); box.appendChild(h);
    d.open.forEach(function (b) { box.appendChild(briefEl(b)); });
    if (d.asks.length) {
      box.appendChild(el('p', 'r-note', st.key === 'read' ? 'Asked by a leg, with no brief yet:' : 'Asked by a leg, in its own words. When the run is read, each becomes a brief on the Decide page or is marked as not yours:'));
      d.asks.forEach(function (a) { box.appendChild(askEl(run, a)); });
    }
    if (!k) box.appendChild(el('p', 'r-none', run.state === 'ended' ? 'Nothing from this run waits on you.' : 'Nothing so far.'));
    d.likely.forEach(function (b) { box.appendChild(briefEl(b)); });
    var past = d.decided.concat(d.from);
    if (past.length || d.notHis.length) {
      var f = fold(run.id + '|past', 'r-past', el('span', null, [past.length ? T.n(past.length, '1 decision of yours', 'decisions of yours') + ' around this run' : '', d.notHis.length ? T.n(d.notHis.length, '1 thing a leg asked that is not yours', 'things a leg asked that are not yours') : ''].filter(Boolean).join(' · ')), false);
      past.forEach(function (b) { f.appendChild(briefEl(b)); });
      d.notHis.forEach(function (a) { var p = askEl(run, a); p.appendChild(el('p', 'r-ask-no', 'Not yours: ' + a.no)); f.appendChild(p); });
      box.appendChild(f);
    }
    return box;
  }
  // what was done: the report, or that there is none yet and how to get one
  function didEl(run, said) {
    var box = el('section', 'r-did'), rep = run.report, st = T.reading(run, window.RUNS, said);
    box.appendChild(el('h4', 'r-h', 'What was done'));
    if (rep) {
      box.appendChild(el('p', 'r-did-text', rep.did));
      var l = el('p', 'r-left'); l.appendChild(el('b', null, 'Left: ')); l.appendChild(document.createTextNode(rep.left)); box.appendChild(l);
      if (rep.shots && rep.shots.length) {
        var shots = el('div', 'r-shots');
        rep.shots.forEach(function (s) { var f = el('figure'), a = link(null, null, s.src, 'Open it full size'), i = el('img'); i.loading = 'lazy'; i.alt = s.caption; i.src = s.src; a.appendChild(i); f.appendChild(a); f.appendChild(el('figcaption', null, s.caption)); shots.appendChild(f); });
        box.appendChild(shots);
      }
      box.appendChild(el('p', 'r-read', 'Written up by the run reader, ' + st.label + ', from the run\'s records.'));
      return box;
    }
    if (run.state !== 'ended') { box.appendChild(el('p', 'r-bare', 'The run has not ended. Its units so far are below; it is written up when it ends.')); return box; }
    box.appendChild(el('p', 'r-bare', st.label));
    if (st.ask && Bd) {
      var b = el('button', 'r-act', 'Have it read'); b.type = 'button'; b.title = 'Leaves the note: ' + T.READ + ' An agent then writes its report and puts its decisions on the Decide page.';
      b.addEventListener('click', function () { Bd.note(T.subject(run), T.READ); }); box.appendChild(b);
    }
    return box;
  }
  function runEl(run, now) {
    var said = saidOf(run), e = T.ended(run), k = T.waits(run), card = el('article', 'r-run r-' + run.state + (k ? ' r-waits' : '') + (run.silent ? ' r-silent' : '')); card.id = 'run-' + run.id;
    var top = el('header', 'r-top'), wh = el('p', 'r-when');
    if (run.state === 'going' && !run.silent) wh.appendChild(el('span', 'k-live'));
    wh.appendChild(document.createTextNode(T.when(run, now))); top.appendChild(wh);
    top.appendChild(el('h3', null, T.title(run)));
    var tw = T.tallyWords(run), cr = T.crew(run);
    top.appendChild(chips([k ? { text: T.n(k, '1 waits on you', 'wait on you'), cls: 'r-chip-you' } : null].concat(tw.map(function (w) { return { text: w, cls: /failed|blocked/.test(w) ? 'r-chip-bad' : /passed/.test(w) ? 'r-chip-ok' : '' }; }),
      [T.n(run.legs, '1 leg', 'legs'), T.money(run.usd) + (run.unpriced ? ' (' + T.n(run.unpriced, '1 leg', 'legs') + ' unpriced)' : '')], cr.map(function (c) { return c.role + ' ×' + c.legs; }),
      [run.station ? 'on ' + run.station : '', run.by ? 'started by ' + run.by : '', run.refusals ? T.n(run.refusals, '1 refusal by the guard', 'refusals by the guard') : ''])));
    var end = el('p', 'r-end'); end.appendChild(el('b', null, run.state === 'ended' ? 'Why it ended: ' : 'Now: ')); end.appendChild(document.createTextNode(e.line + (run.state !== 'ended' && run.now_on ? ' · on ' + run.now_on : '')));
    if (e.told) end.appendChild(el('span', 'r-told', e.told));
    top.appendChild(end);
    if (Bd) { var nb = Bd.button(T.subject(run)); nb.classList.add('r-notes'); top.appendChild(nb); }
    card.appendChild(top);
    var body = el('div', 'r-body'); body.appendChild(didEl(run, said)); body.appendChild(youEl(run, said)); card.appendChild(body);
    var us = fold(run.id + '|units', 'r-units', el('span', null, T.n(run.units.length, '1 unit', 'units') + ', and the agent that worked each'), true), list = el('div', 'r-unit-list');
    run.units.forEach(function (u) { list.appendChild(unitEl(run, u)); });
    us.appendChild(list); card.appendChild(us);
    if (run.proposals.length) {
      var pf = fold(run.id + '|props', 'r-props', el('span', null, 'Its retrospective\'s proposals for the relay itself (the master\'s to take up)'), false);
      run.proposals.forEach(function (p) { pf.appendChild(el('pre', null, p.text)); }); card.appendChild(pf);
    }
    return card;
  }
  function startsEl(list, now) { var s = T.starts(list), p = el('div', 'r-starts'); p.appendChild(el('b', null, s.top)); p.appendChild(el('span', null, s.why)); return p; }
  function stripEl(into) {
    T.strip(window.RUNS).forEach(function (b) {
      var a = el('a', 'r-bar r-bar-' + b.key); a.href = '#run-' + b.id; a.title = b.tip; a.setAttribute('aria-label', b.tip); a.appendChild(el('span')); a.firstChild.style.height = b.height + '%'; into.appendChild(a);
    });
  }
  function changed(into, sig) { if (into.dataset.sig === sig) return false; into.dataset.sig = sig; into.innerHTML = ''; return true; }
  function draw() {
    var R = window.RUNS || null, now = Date.now() / 1000, rows = T.rows(R);
    if (window.OPS && C && C.nowPill) C.nowPill(window.OPS);
    var cnt = document.getElementById('r-count'); if (cnt) cnt.textContent = T.head(R);
    var foot = document.getElementById('r-foot'); if (foot) foot.textContent = T.foot(R);
    var warn = T.warnings(R, now), w = document.getElementById('r-warn');
    if (w && changed(w, JSON.stringify(warn))) { w.hidden = !warn.length; warn.forEach(function (x) { w.appendChild(el('p', null, x)); }); }
    var tgl = document.getElementById('r-only'); if (tgl) { tgl.setAttribute('aria-pressed', only ? 'true' : 'false'); tgl.hidden = !rows.length; }
    var notes = rows.map(function (x) { return x.run ? saidOf(x.run) : 0; });
    var mine = (window.BRIEFS || []).map(function (b) { var s = Br && Br.said ? Br.said(b) : null; return [b.id, b.state, s && s.id, s && s.state, b.waits]; });
    var live = rows.some(function (x) { return x.run && x.run.state !== 'ended'; });          // a run that is going says for how long: drawn again each minute
    var sig = JSON.stringify([R && R.runs, R && R.reading, R && R.repo, notes, mine, only, live ? Math.floor(now / 60) : 0]);
    var typing = board.contains(document.activeElement) && document.activeElement.tagName === 'INPUT' && document.activeElement.value;
    var strip = document.getElementById('r-strip');
    if (strip && changed(strip, JSON.stringify(T.strip(R)))) stripEl(strip);
    if (typing || !changed(board, sig)) return;          // not while he is typing an answer of his own
    var shown = rows.filter(function (x) { return !only || (x.run && T.waits(x.run)); });
    if (!shown.length) board.appendChild(el('p', 'r-none', !R ? 'The runs have not been read yet.' : only ? 'No run of the last ' + (R.most || 20) + ' has anything waiting on you.' : 'No relay run is on the pipeline board yet.'));
    shown.forEach(function (x) { board.appendChild(x.run ? runEl(x.run, now) : startsEl(x.starts, now)); });
    if (location.hash && !draw.jumped) { draw.jumped = true; var at = document.getElementById(location.hash.slice(1)); if (at) at.scrollIntoView(); }
  }
  var tg = document.getElementById('r-only');
  if (tg) tg.addEventListener('click', function () { only = !only; draw(); });
  if (C && C.live) C.live(draw); else draw();
  if (Bd && Bd.onchange) Bd.onchange(draw);
  setInterval(draw, 60000);       // a reading that stops is told by the page's own clock
})();
