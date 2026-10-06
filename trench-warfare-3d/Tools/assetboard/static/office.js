// The live half of the overview and of the branches page: the pulse (who is at work, what needs the owner), the
// office (a room per open branch: a room someone works in is full width with its frogs busy at their desks, the
// branch's board items on a whiteboard and the assets it touches on the wall; a room nobody sits in is one line),
// the lounge (the crew nothing calls, dozing), and on each asset card the frogs at work on a branch that touches it.
// Every element is optional, so a page draws the parts it has. Reads data/ops.js (ops.py; --watch keeps it fresh)
// every 20 seconds and redraws a part only when its data changed, so the loops keep playing. Frogs: crew.js.
(function () {
  var C = window.Crew, el = C && C.el;
  if (!C) return;
  function $(id) { return document.getElementById(id); }
  function set(id, text) { var e = $(id); if (e) e.textContent = text; }
  function shortBranch(b) { return b.replace(/^lane\/(show|sim)\//, ''); }
  function laneKind(b) { var m = /^lane\/(show|sim)\//.exec(b); return m ? m[1] : b.split('/')[0]; }
  function changed(node, sig) { if (!node || node.dataset.sig === sig) return false; node.dataset.sig = sig; node.innerHTML = ''; return true; }
  // the first phrase of a board item's title, at most ~58 characters, cut at a word
  function headline(t) {
    var h = t.split(/\s\(|,\s|\s[—–]\s|;\s/)[0];
    var c = h.indexOf(': '); if (c > 0 && h.slice(0, c).split(/\s+/).length >= 4) h = h.slice(0, c);
    if (h.length > 58) h = h.slice(0, 58).replace(/\s+\S*$/, '') + '…';
    return h;
  }
  // how long a ready stage has waited, from the first reading that saw it ready
  function waited(since) {
    if (!since || !window.OPS_NOW) return '';
    var m = Math.round((new Date(window.OPS_NOW.replace(' ', 'T')) - new Date(since.replace(' ', 'T'))) / 60000);
    return m < 2 ? ' · just now' : m < 60 ? ' · waiting ' + m + ' min' : m < 2880 ? ' · waiting ' + Math.round(m / 60) + ' h' : ' · waiting ' + Math.round(m / 1440) + ' d';
  }
  function said(subject) {
    var w = subject.replace(/^(tools|docs|sim|show|test)[^:]*:\s*/i, '').replace(/^[\w.\/-]+\.\w{1,5}:\s*/, '').replace(/^the board's [\w-]+ review round:\s*/i, '').split(/[,;:]\s/)[0];
    return w.charAt(0).toUpperCase() + w.slice(1);
  }
  function ago(day) {
    var d = Math.round((Date.now() - new Date(day + 'T12:00:00').getTime()) / 864e5);
    return d <= 0 ? 'today' : d === 1 ? 'yesterday' : d + ' days ago';
  }
  function slug(b) { return 'room-' + b.replace(/[^a-z0-9]+/gi, '-'); }
  var onBranches = !!document.getElementById('strip');
  function readyOf(l) { var out = []; l.items.forEach(function (it) { it.stages.forEach(function (s) { if (s.state === 'READY') out.push({ item: it, stage: s, lane: l }); }); }); return out; }

  var Bd = window.Board;
  function laneSubject(l) {
    var links = [{ label: 'Its room on the branches page', href: 'floor.html#' + slug(l.branch) }];
    l.items.forEach(function (it) { links.push({ label: 'Board item: ' + headline(it.title || it.id), href: 'process.html' }); });
    var facts = [laneKind(l.branch), (l.ahead || 0) + ' ahead'].concat(l.dirty ? [l.dirty + ' files changing'] : [], l.checkout ? ['checked out in ' + l.checkout] : [], l.last[0] ? ['last change ' + ago(l.last[0].date)] : []);
    return { kind: 'lane', id: l.branch, kindLabel: 'branch', title: shortBranch(l.branch), sub: l.last[0] ? said(l.last[0].subject) : '', lane: l.branch, assets: l.assets, links: links, facts: facts };
  }
  function desk(w, l) {
    var d = el('div', 'k-desk ' + (w.state || 'idle'));
    if (Bd && l) {
      d.classList.add('b-click'); d.tabIndex = 0; d.setAttribute('role', 'button');
      var s = laneSubject(l); s.kind = 'worker'; s.id = w.id; s.kindLabel = w.kind === 'session' ? 'Claude session' : w.kind; s.title = C.title(w); s.sub = C.doing(w) || w.what || '';
      s.facts = [w.state === 'working' ? 'at work' : 'resting', 'on ' + shortBranch(l.branch)]; s.links = [{ label: 'Where it is in the house', href: 'house.html?pin=' + encodeURIComponent(w.id) }].concat(s.links);
      // on the control screen the house is on the page: the click picks this worker's frog there, and the profile
      // beside the house says the rest; a frog the house does not have (or a page without it) gets the panel
      var show = function () {
        var V = window.HouseView, deck = $('profile') && $('deck');
        if (deck && V && V.pin && V.pin(w.uid || w.id)) { deck.scrollIntoView({ behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth' }); return; }
        Bd.open(s);
      };
      d.addEventListener('click', show); d.addEventListener('keydown', function (e) { if (e.key === 'Enter') show(); });
    }
    d.appendChild(C.booth(w));
    var tag = el('div', 'k-nametag');
    tag.appendChild(el('b', null, C.title(w)));
    var line = C.doing(w) || (w.kind === 'session' ? '' : w.what) || '';
    var kk = el('span', 'k-kind'); tag.appendChild(kk);
    if (line) kk.appendChild(el('span', 'k-doing', line));
    else kk.textContent = w.kind === 'session' ? (w.state === 'working' ? 'at work' : C.ago(w.age || 0)) : w.kind;
    d.appendChild(tag);
    d.title = C.title(w) + (w.what ? '\n' + w.what : '');
    return d;
  }
  function tags(l, working, ready) {
    var meta = el('div', 'k-tags');
    if (working) meta.appendChild(el('span', 'k-tag k-hot', working + ' at work'));
    if (ready) meta.appendChild(el('span', 'k-tag k-ready', ready + ' ready'));
    if (l.dirty) { var d = el('span', 'k-tag', l.dirty + ' files changing'); d.title = l.dirty_files.join('\n'); meta.appendChild(d); }
    if (l.ahead) meta.appendChild(el('span', 'k-tag k-quiet', l.ahead + ' ahead'));
    return meta;
  }
  function title(l) {
    var t = el('div', 'k-room-title');
    t.appendChild(el('span', 'k-lane k-lane-' + laneKind(l.branch), laneKind(l.branch)));
    t.appendChild(el('h3', null, shortBranch(l.branch)));
    if (Bd) t.appendChild(Bd.button(laneSubject(l)));
    return t;
  }
  function board(l) {
    var wb = el('div', 'k-board');
    l.items.forEach(function (it) {
      var row = el('div', 'k-item');
      row.appendChild(el('b', null, it.title || it.id));
      var st = el('div', 'k-stages');
      it.stages.forEach(function (s) {
        var p = el('span', 'k-stage st-' + s.state, s.id);
        p.title = s.id + ': ' + s.state.toLowerCase() + ' · ' + (s.skill || s.role || 'no role') + (s.station ? ' · ' + s.station : '');
        st.appendChild(p);
      });
      row.appendChild(st);
      wb.appendChild(row);
    });
    return wb;
  }
  function wall(l, n) {
    var w = el('div', 'k-wall');
    l.assets.slice(0, n).forEach(function (a) {
      var x = el('a', 'k-frame'); x.href = 'a/' + a.id + '.html'; x.title = a.name + ' · ' + a.status;
      if (a.pic) { var i = el('img'); i.src = a.pic; i.loading = 'lazy'; i.alt = ''; x.appendChild(i); }
      x.appendChild(el('span', null, a.name));
      w.appendChild(x);
    });
    if (l.assets.length > n) w.appendChild(el('span', 'k-more', '+' + (l.assets.length - n)));
    return w;
  }
  // a room someone works in: full width, the desks large, the board and the wall beside them
  function room(l) {
    var working = l.workers.filter(function (w) { return w.state === 'working'; }).length;
    var r = el('article', 'k-room' + (working ? ' busy' : '') + (l.workers.length > 1 ? ' wide' : ' half')); r.id = slug(l.branch);
    var h = el('header'); h.appendChild(title(l)); h.appendChild(tags(l, working, readyOf(l).length)); r.appendChild(h);
    if (l.items.length) r.appendChild(board(l));
    var body = el('div', 'k-room-body' + (onBranches ? ' k-detail' : ''));
    var desks = el('div', 'k-desks' + (onBranches ? '' : ' k-desks-sm'));
    l.workers.slice().sort(function (a, b) { return (b.state === 'working') - (a.state === 'working') || C.title(a).localeCompare(C.title(b)); }).forEach(function (w) { desks.appendChild(desk(w, l)); });
    body.appendChild(desks);
    var side = el('div', 'k-room-side');
    if (l.assets.length) side.appendChild(wall(l, 8));
    if (onBranches && l.dirty_files && l.dirty_files.length) {
      var ch = el('div', 'k-files'); ch.appendChild(el('b', null, l.dirty + ' files changing'));
      var chips = el('div', 'k-chips');
      l.dirty_files.slice(0, 6).forEach(function (f) { var c = el('span', 'k-chip k-ext-' + (f.split('.').pop() || '').toLowerCase(), f.split('/').pop()); c.title = f; chips.appendChild(c); });
      if (l.dirty_files.length > 6) chips.appendChild(el('span', 'k-chip k-more-chip', '+' + (l.dirty_files.length - 6)));
      ch.appendChild(chips);
      side.appendChild(ch);
    }
    if (onBranches && l.last.length > 1) {
      var cm = el('div', 'k-files'); cm.appendChild(el('b', null, 'Last commits'));
      var tl = el('ol', 'k-tl');
      var lastDay = '';
      l.last.slice(0, 3).forEach(function (k) {
        var li = el('li'); li.appendChild(el('span', 'k-tl-what', said(k.subject)));
        if (k.date !== lastDay) li.appendChild(el('span', 'k-tl-when', ago(k.date))); lastDay = k.date;
        li.title = k.date + ' · ' + k.subject; tl.appendChild(li);
      });
      cm.appendChild(tl);
      side.appendChild(cm);
    }
    if (side.children.length) body.appendChild(side); else body.classList.add('solo');
    r.appendChild(body);
    var lk = l.last.filter(function (k) { return !/^(tools|docs)\b/i.test(k.subject); })[0] || l.last[0];
    if (lk && !onBranches) { var lc = el('p', 'k-last', headline(said(lk.subject)) + ' · ' + ago(lk.date)); lc.title = lk.subject; r.appendChild(lc); }
    return r;
  }
  // a room nobody sits in: one line, and what waits in it
  function quietRoom(l) {
    var ready = readyOf(l);
    var r = el('article', 'k-room k-room-line' + (ready.length ? ' waits' : '')); r.id = slug(l.branch);
    r.appendChild(title(l));
    r.appendChild(tags(l, 0, ready.length));
    if (ready.length) r.appendChild(el('p', 'k-waits', 'waits on ' + ready.map(function (x) { return x.stage.id; }).join(', ') + ' · ' + (ready[0].item.title || ready[0].item.id)));
    else if (l.items.length) r.appendChild(el('p', 'k-waits', l.items.map(function (it) { return it.title || it.id; }).join(' · ')));
    return r;
  }

  // the office grid's last card takes whatever its row has left, so no row ends in a hole
  function fillRow() {
    var g = document.querySelector('.k-floorgrid'); if (!g) return;
    var cols = getComputedStyle(g).gridTemplateColumns.split(' ').length;
    var cards = g.querySelectorAll('.k-room'); if (!cards.length) return;
    cards.forEach(function (c) { c.style.gridColumn = ''; });
    if (g.querySelector('.k-room.half')) return;   // a two-row room: the grid packs the lines beside it by itself
    var used = 0;
    cards.forEach(function (c) { c.style.gridColumn = ''; });
    cards.forEach(function (c, i) {
      var span = c.classList.contains('wide') ? cols : c.classList.contains('half') ? Math.min(2, cols) : 1;
      if (used % cols + span > cols) used += cols - used % cols;   // it wraps to a new row
      used += span;
      if (i === cards.length - 1 && used % cols) c.style.gridColumn = 'span ' + (span + cols - used % cols);
    });
  }
  window.addEventListener('resize', fillRow);

  function draw() {
    var o = window.OPS;
    if (!o) return;
    var office = $('office'); if (office) office.hidden = false;
    C.nowPill(o);
    var all = C.everyone(o);
    var RANK = { session: 0, skill: 1, agent: 2, role: 3, machine: 4 };
    function steady(a, b) { return (RANK[a.w.kind] || 9) - (RANK[b.w.kind] || 9) || (a.where || '').localeCompare(b.where || '') || C.title(a.w).localeCompare(C.title(b.w)); }
    var working = all.at.filter(function (x) { return x.w.state === 'working'; }).sort(steady);
    var open = o.lanes.filter(function (l) { return l.workers.length || l.items.length || l.dirty; });
    var ready = []; o.lanes.forEach(function (l) { ready = ready.concat(readyOf(l)); });
    ready.sort(function (a, b) { return (a.stage.since || '~').localeCompare(b.stage.since || '~'); });

    // the pulse: "Needs you" is the owner queue (src_queue.py), and its number is the number of rows the queue lists.
    // Before the first queue.js is written it is the board's ready stages, as it was.
    var Q = window.OwnerQueue;
    var queue = window.QUEUE || { ready: ready.map(function (x) {
      return { title: x.item.title || x.item.id, item: x.item.id, stage: x.stage.id, lane: x.lane.branch, role: x.stage.skill || x.stage.role || '', days: null }; }) };
    var groups = Q ? Q.groups(queue) : [], waiting = groups.reduce(function (n, g) { return n + g.rows.length; }, 0);
    function qrow(g, r, cls, m) {
      // a decision leads to its brief on the decisions page (decide.js), or to its place among those without one
      var Bf = g.key === 'decide' ? window.Briefs : null, bf = Bf ? Bf.match(window.BRIEFS, r.top) : null, to = Bf ? 'decide.html#' + (bf ? bf.id : 'q-' + Bf.slug(r.top)) : '';
      var a = el(r.lane || r.url || to ? 'a' : 'div', cls); a.title = r.tip || r.top;
      if (to) a.href = to; else if (r.url) a.href = r.url; else if (r.lane) a.href = (onBranches ? '' : 'floor.html') + '#' + slug(r.lane);
      var what = el('span', 'k-take-what');
      what.appendChild(el('span', 'k-take-top', g.key === 'ready' ? headline(r.top) : r.top));
      var sub = el('span', 'k-take-sub'); r.chips.forEach(function (c) { sub.appendChild(el('span', 'k-qchip', c)); }); what.appendChild(sub);
      if (bf) sub.appendChild(el('span', 'k-qchip k-qbrief', 'brief, with ' + (bf.evidence.length ? bf.evidence.length + (bf.evidence.length === 1 ? ' picture' : ' pictures') : 'no picture')));
      a.appendChild(what);
      m = m || {};
      // the pictures of a row: what the board holds for it, then what the briefs about its lane show
      var shots = (m.shots || []).slice();
      (window.BRIEFS || []).forEach(function (b) { if (r.lane && b.lane === r.lane) (b.evidence || []).forEach(function (e) { if (e.src && e.kind !== 'film' && shots.length < 3) shots.push({ src: e.src, name: e.file, caption: e.caption }); }); });
      var has = m.detail && m.detail.length;          // the lines say it: then no tip under the title, and a ready step by its short name
      var subj = { kind: 'queue', id: g.key + ': ' + r.top, kindLabel: g.label.toLowerCase(), title: g.key === 'ready' ? headline(r.top) : r.top, sub: !has && r.tip && r.tip !== r.top ? r.tip : '', lane: r.lane || '',
        links: (bf ? [{ label: 'Its brief, with the options', href: to }] : []).concat(r.lane ? [{ label: 'Its branch, ' + shortBranch(r.lane), href: 'floor.html#' + slug(r.lane) }] : r.url ? [{ label: 'Open it', href: r.url }] : []),
        facts: r.chips, detail: m.detail || [], shots: shots, actions: m.actions || [], wantsShots: g.key === 'land' || g.key === 'ready' || g.key === 'approved' };
      // a click on the row opens what it is, with its pictures and what he can say should happen; a decision with a brief
      // goes to the brief, which is that page already. A click with a key held still follows the link.
      if (Bd && !bf) a.addEventListener('click', function (ev) { if (ev.ctrlKey || ev.metaKey || ev.shiftKey || ev.button) return; ev.preventDefault(); Bd.open(subj); });
      if (Bd) a.appendChild(Bd.button(subj));
      return a;
    }
    var pr = $('p-ready');
    if (pr && pr.textContent !== String(waiting) && pr.textContent !== '–') { var nc = $('p-needs'); nc.classList.remove('k-bump'); void nc.offsetWidth; nc.classList.add('k-bump'); }
    set('p-ready', waiting);
    var list = $('p-ready-list'), sig = JSON.stringify([groups, (window.BRIEFS || []).map(function (b) { return [b.id, b.state]; })]);
    if (changed(list, sig)) {
      groups.forEach(function (g) {
        var row = el('a', 'k-take k-take-line k-chip-' + g.key); row.href = '#q-' + g.key;          // to its own card of the queue
        var what = el('span', 'k-take-what');
        what.appendChild(el('span', 'k-take-top', g.label));
        what.appendChild(el('span', 'k-take-sub', g.rows.slice(0, 2).map(function (r) { return r.top; }).join(' · ') + (g.rows.length > 2 ? ' · +' + (g.rows.length - 2) : '')));
        row.appendChild(what); row.appendChild(el('b', null, String(g.rows.length)));
        list.appendChild(row);
      });
      if (!waiting) list.appendChild(el('span', null, 'nothing waits on you'));
    }
    var needs = $('p-needs'); if (needs) needs.classList.toggle('calm', !waiting);
    // the queue itself: a card per group, a row of chips per entry, the oldest first; five rows, the rest folded
    var cards = $('queue-cards'), qs = $('queue');
    if (qs) qs.hidden = !waiting;
    if (changed(cards, sig)) {
      groups.forEach(function (g) {
        var card = el('article', 'k-qcard k-q-' + g.key); card.id = 'q-' + g.key;
        var h = el('header'); h.appendChild(el('b', null, g.label)); h.appendChild(el('span', 'k-qn', String(g.rows.length))); card.appendChild(h);
        g.rows.slice(0, 5).forEach(function (r, i) { card.appendChild(qrow(g, r, 'k-qrow', g.more && g.more[i])); });
        if (g.rows.length > 5) {
          var more = el('details', 'k-qmore'); more.appendChild(el('summary', null, 'All ' + g.rows.length));
          g.rows.slice(5).forEach(function (r, i) { more.appendChild(qrow(g, r, 'k-qrow', g.more && g.more[i + 5])); });
          card.appendChild(more);
        }
        cards.appendChild(card);
      });
    }
    set('p-at', working.length);
    var dots = $('p-at-dots');
    if (changed(dots, working.map(function (x) { return x.w.id; }).join(',') + '|' + all.idle.length)) {
      if (working.length) working.slice(0, 8).forEach(function (x) { var f = C.frog(x.w, 36); f.title = C.title(x.w); dots.appendChild(f); });
      else if (o.lanes.some(function (l) { return l.last.length; })) {
        var game = function (l) { return l.last.length && !/^(tools|docs)\b/i.test(l.last[0].subject); };
        var pool = o.lanes.filter(game); if (!pool.length) pool = o.lanes.filter(function (l) { return l.last.length; });
        var lastL = pool.sort(function (a, b) { return b.last[0].date.localeCompare(a.last[0].date); })[0];
        var nx = el('a', 'k-next'); nx.href = (onBranches ? '' : 'floor.html') + '#' + slug(lastL.branch);
        nx.appendChild(el('span', 'k-soft', 'Last change · ' + shortBranch(lastL.branch) + ' · ' + ago(lastL.last[0].date)));
        nx.appendChild(el('b', null, headline(said(lastL.last[0].subject)))); dots.appendChild(nx); }
      else { var nf = matchMedia('(max-width: 760px)').matches ? 3 : 5; all.idle.slice(0, nf).forEach(function (r) { var f = C.frog({ id: r.id, kind: r.kind, name: r.name, state: 'idle' }, 30); f.title = C.label(r.name) + ' (asleep)'; dots.appendChild(f); });
        if (all.idle.length > nf) dots.appendChild(el('span', 'k-dots-more', '+' + (all.idle.length - nf))); }
    }
    var lb2 = $('p-rooms-bar');
    if (lb2) { var lv = o.lanes.filter(function (l) { return l.live; }).length; lb2.innerHTML = '<i style="flex:' + open.length + '" class="open"></i><i style="flex:' + Math.max(0, lv - open.length) + '" class="live"></i><i style="flex:' + (o.lanes.length - lv) + '" class="parked"></i>'; }
    var atCard = $('p-at'); if (atCard) atCard.closest('.k-stat').classList.toggle('quiet', !working.length);
    var nm = working.filter(function (x) { return x.w.kind === 'machine'; }).length;
    set('p-at-sub', (working.length - nm) + ' crew · ' + nm + ' machine' + (nm === 1 ? '' : 's') + ' · ' + all.idle.length + ' asleep');
    set('p-idle', all.idle.length);
    set('p-idle-sub', 'of ' + o.roster.length + ' skills and agents');
    set('p-rooms', open.length);
    var nLive = o.lanes.filter(function (l) { return l.live; }).length;
    set('p-rooms-sub', open.length + ' open · ' + Math.max(0, nLive - open.length) + ' live · ' + (o.lanes.length - nLive) + ' parked');
    var f = Q ? Q.fresh(window.BEAT, Date.now()) : { stale: false };
    set('stamp', f.stale ? f.text + ': the watcher has stopped (ops.py --watch 20)' : 'read ' + (window.BEAT || window.OPS_NOW).replace('T', ' ') + ', every 20 s');
    var st = $('stamp'); if (st) st.parentNode.classList.toggle('stale', f.stale);

    // the hero: everyone at work, a frog each
    var row = $('crewrow');
    if (row && changed(row, JSON.stringify([working.map(function (x) { return [x.w.id, x.where, x.w.doing]; }), working.length ? 0 : all.idle.length, ready.length]))) {
      row.classList.remove('asleep');
      row.classList.toggle('n1', working.length === 1); row.classList.toggle('n2', working.length === 2);
      working.slice(0, 3).forEach(function (x) {
        var f = el('a', 'k-mate'); f.href = (onBranches ? '' : 'floor.html') + '#' + slug(x.branches[0] || '');
        f.appendChild(C.booth(x.w));
        var cap = el('span', 'k-mate-cap'); var head = el('span', 'k-mate-head'); var nm = el('b', null, C.title(x.w)); head.appendChild(nm); cap.appendChild(head);
        var lane = o.lanes.filter(function (l) { return l.branch === x.branches[0]; })[0];
        var task = C.doing(x.w) || (x.w.kind === 'session' ? '' : x.w.what) || (lane && lane.items[0] ? headline(lane.items[0].title || lane.items[0].id) : '') || (lane && lane.last[0] ? headline(said(lane.last[0].subject)) : '');
        if (task) cap.appendChild(el('span', 'k-mate-task', task));
        var foot = el('span', 'k-mate-foot'); foot.appendChild(el('span', 'k-mate-where', shortBranch(x.where))); foot.appendChild(el('span', 'k-open', 'Open room →'));
        cap.appendChild(foot);
        var dots3 = el('span', 'k-typing'); dots3.innerHTML = '<i></i><i></i><i></i>'; foot.insertBefore(dots3, foot.firstChild);
        f.appendChild(cap); f.title = C.title(x.w) + ' · ' + x.where + (x.w.what ? '\n' + x.w.what : '');
        row.appendChild(f);
      });
      if (working.length > 3) row.appendChild(el('a', 'k-more', '+' + (working.length - 3) + ' more at work')).href = '#office';
      if (!working.length) {           // nobody at work: the hero shows the crew asleep, and what waits
        row.classList.add('asleep');
        all.idle.slice(0, 6).forEach(function (r) {
          var f = el('div', 'k-sleeper'); f.title = C.label(r.name) + ': ' + r.does;
          f.appendChild(C.booth({ id: r.id, kind: r.kind, name: r.name, state: 'idle' })); f.appendChild(el('span', null, C.label(r.name)));
          row.appendChild(f);
        });
        if (all.idle.length > 6) { var mt = el('a', 'k-sleeper k-sleeper-more'); mt.href = 'floor.html'; mt.title = all.idle.slice(6).map(function (r) { return C.label(r.name); }).join(', ');
          var mos = el('div', 'k-mosaic'); all.idle.slice(6, 10).forEach(function (r) { var i = el('img'); i.src = 'img/crew/' + C.key({ id: r.id, kind: r.kind, name: r.name }).replace(/^~/, '') + '.jpg'; i.alt = ''; mos.appendChild(i); });
          mos.appendChild(el('span', 'k-mosaic-n', '+' + (all.idle.length - 6))); mt.appendChild(mos); mt.appendChild(el('span', null, 'more asleep')); row.appendChild(mt); }
        row.appendChild(el('p', 'k-asleep-line', 'Everyone is asleep' + (ready.length ? ' — ' + ready.length + ' stage' + (ready.length > 1 ? 's wait' : ' waits') + ' for you.' : '.')));
      }
    }

    // the rooms: worked-in rooms large, the rest one line each, the ones that wait on the owner first
    var busy = open.filter(function (l) { return l.workers.length; }).sort(function (a, b) { return b.workers.length - a.workers.length || a.branch.localeCompare(b.branch); });
    var still = open.filter(function (l) { return !l.workers.length; })
      .sort(function (a, b) { return readyOf(b).length - readyOf(a).length || b.items.length - a.items.length; });
    var rooms = $('rooms');
    if (changed(rooms, JSON.stringify([busy, all.at.map(function (x) { return [x.w.id, x.w.state, x.where]; })]))) {
      busy.forEach(function (l) { rooms.appendChild(room(l)); });
      // skills at work on a branch with no room of its own (a board stage on a branch not checked out)
      all.at.forEach(function (x) {
        if (x.w.kind === 'session' || x.w.kind === 'machine') return;
        if (busy.some(function (l) { return x.branches.indexOf(l.branch) >= 0; })) return;
        rooms.appendChild(room({ branch: x.where, workers: [x.w], items: [], assets: [], last: [], dirty: 0, ahead: 0, dirty_files: [] }));
      });
      rooms.hidden = !rooms.children.length;
    }
    var lines = $('rooms-quiet');
    var cap = onBranches ? still.length : 3;      // the overview shows the first few; the branches page all
    if (changed(lines, JSON.stringify([still, cap]))) {
      still.slice(0, cap).forEach(function (l) { lines.appendChild(quietRoom(l)); });
      if (!onBranches) { var more = el('a', 'k-room k-room-line k-room-more'); more.href = 'floor.html'; more.appendChild(el('b', null, 'All ' + (o.lanes.length) + ' branches →')); more.appendChild(el('span', 'k-waits', (still.length > cap ? (still.length - cap) + ' more open, ' : '') + (o.lanes.length - open.length) + ' quiet')); lines.appendChild(more); }
    }

    // the lounge: a strip of sleeping frogs, what each is for on hover
    var naps = $('naps');
    var heroSleeps = row && !working.length ? 6 : 0, lounge = all.idle.slice(heroSleeps);
    if (changed(naps, lounge.map(function (r) { return r.id; }).join(','))) {
      var capL = heroSleeps ? 0 : 9;
      lounge.slice(0, capL).forEach(function (r) {
        var n = el('div', 'k-nap'); n.title = C.label(r.name) + ': ' + r.does;
        n.appendChild(C.booth({ id: r.id, kind: r.kind, name: r.name, state: 'idle', what: r.does }, 'k-round'));
        n.appendChild(el('b', null, C.label(r.name)));
        naps.appendChild(n);
      });
      if (lounge.length > capL) { var mo = el('a', 'k-nap k-nap-more'); mo.href = onBranches ? '#lounge' : 'floor.html#lounge'; mo.title = lounge.slice(capL).map(function (r) { return C.label(r.name); }).join(', '); mo.appendChild(el('span', 'k-nap-n', '+' + (lounge.length - capL))); mo.appendChild(el('b', null, 'more')); naps.appendChild(mo); }
    }
    set('n-lounge', (heroSleeps ? lounge.length + ' more' : all.idle.length) + ' asleep');
    set('n-asleep', all.idle.length + ' asleep in the lounge →');
    var pill = $('n-asleep'); if (pill) pill.closest('p').hidden = !all.idle.length;
    var lg = $('lounge'); if (lg) lg.hidden = !lounge.length || !!heroSleeps;

    // the cards: frogs at work on a branch that touches the asset
    var on = {}, assetsOf = {};
    o.lanes.forEach(function (l) { assetsOf[l.branch] = l.assets.map(function (a) { return a.id; }); });
    working.forEach(function (x) {
      x.branches.forEach(function (b) { (assetsOf[b] || []).forEach(function (id) { (on[id] = on[id] || []).push(x); }); });
    });
    document.querySelectorAll('.card .on-it').forEach(function (spot) {
      var who = on[spot.dataset.asset] || [];
      if (!changed(spot, who.map(function (x) { return x.w.id; }).join(','))) return;
      spot.closest('.card').classList.toggle('worked', who.length > 0);
      who.slice(0, 3).forEach(function (x) { var f = C.frog(x.w, 44); f.title += ' · on ' + x.where; spot.appendChild(f); });
      if (who.length > 3) spot.appendChild(el('span', 'more', '+' + (who.length - 3)));
    });
    fillRow();
    if (window.drawBranches) window.drawBranches(o);
  }
  C.live(draw);
})();
