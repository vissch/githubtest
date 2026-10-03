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
  function slug(b) { return 'room-' + b.replace(/[^a-z0-9]+/gi, '-'); }
  var onBranches = !!document.getElementById('strip');
  function readyOf(l) { var out = []; l.items.forEach(function (it) { it.stages.forEach(function (s) { if (s.state === 'READY') out.push({ item: it, stage: s, lane: l }); }); }); return out; }

  function desk(w) {
    var d = el('div', 'k-desk ' + (w.state || 'idle'));
    d.appendChild(C.booth(w));
    var tag = el('div', 'k-nametag');
    tag.appendChild(el('b', null, C.title(w)));
    var line = w.doing || (w.kind === 'session' ? '' : w.what) || '';
    tag.appendChild(el('span', 'k-kind', (w.kind === 'session' ? (w.state === 'working' ? 'Claude' : 'Claude · ' + C.ago(w.age || 0)) : w.kind) + (line ? ' · ' : '')));
    if (line) tag.lastChild.appendChild(el('span', 'k-doing', line));
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
    var r = el('article', 'k-room' + (working ? ' busy' : '')); r.id = slug(l.branch);
    var h = el('header'); h.appendChild(title(l)); h.appendChild(tags(l, working, readyOf(l).length)); r.appendChild(h);
    var body = el('div', 'k-room-body');
    var desks = el('div', 'k-desks');
    l.workers.slice().sort(function (a, b) { return (b.state === 'working') - (a.state === 'working'); }).forEach(function (w) { desks.appendChild(desk(w)); });
    body.appendChild(desks);
    var side = el('div', 'k-room-side');
    if (l.items.length) side.appendChild(board(l));
    if (l.assets.length) side.appendChild(wall(l, 8));
    if (side.children.length) body.appendChild(side); else body.classList.add('solo');
    r.appendChild(body);
    if (l.last.length) r.appendChild(el('p', 'k-last', 'last commit ' + l.last[0].date + ' · ' + l.last[0].subject));
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

  function draw() {
    var o = window.OPS;
    if (!o) return;
    var office = $('office'); if (office) office.hidden = false;
    C.nowPill(o);
    var all = C.everyone(o);
    var working = all.at.filter(function (x) { return x.w.state === 'working'; });
    var open = o.lanes.filter(function (l) { return l.workers.length || l.items.length || l.dirty; });
    var ready = []; o.lanes.forEach(function (l) { ready = ready.concat(readyOf(l)); });

    // the pulse
    set('p-ready', ready.length);
    var list = $('p-ready-list');
    if (changed(list, JSON.stringify(ready.map(function (x) { return [x.lane.branch, x.stage.id]; })))) {
      ready.slice(0, 3).forEach(function (x) {
        var row = el('a', 'k-take'); row.href = (onBranches ? '' : 'floor.html') + '#' + slug(x.lane.branch);
        var what = el('span', 'k-take-what');
        what.appendChild(el('span', 'k-take-top', x.stage.id + ' · ' + shortBranch(x.lane.branch)));
        what.appendChild(el('span', 'k-take-sub', 'for ' + C.label(x.stage.skill || x.stage.role || 'anyone') + ' · ' + (x.item.title || x.item.id)));
        row.appendChild(what); row.appendChild(el('b', null, 'Take →'));
        row.title = (x.item.title || x.item.id) + ': ' + x.stage.id + ' is ready (' + (x.stage.skill || x.stage.role || 'no role') + ')';
        list.appendChild(row);
      });
      if (!ready.length) list.appendChild(el('span', null, 'nothing waits on you'));
    }
    var needs = $('p-needs'); if (needs) { needs.classList.toggle('calm', !ready.length); needs.href = ready.length ? (onBranches ? '' : 'floor.html') + '#' + slug(ready[0].lane.branch) : '#office'; }
    set('p-at', working.length);
    var atCard = $('p-at'); if (atCard) atCard.closest('.k-stat').classList.toggle('quiet', !working.length);
    var nm = working.filter(function (x) { return x.w.kind === 'machine'; }).length;
    set('p-at-sub', (working.length - nm) + ' crew · ' + nm + ' machine' + (nm === 1 ? '' : 's'));
    set('p-idle', all.idle.length);
    set('p-idle-sub', 'of ' + o.roster.length + ' skills and agents');
    set('p-rooms', open.length);
    set('p-rooms-sub', o.lanes.filter(function (l) { return l.live; }).length + ' live of ' + o.lanes.length + ' branches');
    set('stamp', 'read ' + window.OPS_NOW + ', every 20 s');

    // the hero: everyone at work, a frog each
    var row = $('crewrow');
    if (row && changed(row, JSON.stringify([working.map(function (x) { return [x.w.id, x.where, x.w.doing]; }), working.length ? 0 : all.idle.length, ready.length]))) {
      row.classList.remove('asleep');
      working.slice(0, 4).forEach(function (x) {
        var f = el('a', 'k-mate'); f.href = (onBranches ? '' : 'floor.html') + '#' + slug(x.branches[0] || '');
        f.appendChild(C.booth(x.w));
        var cap = el('span', 'k-mate-cap'); cap.appendChild(el('b', null, C.title(x.w))); cap.appendChild(el('span', null, x.w.doing || shortBranch(x.where)));
        f.appendChild(cap); f.title = C.title(x.w) + ' · ' + x.where + (x.w.what ? '\n' + x.w.what : '');
        row.appendChild(f);
      });
      if (working.length > 4) row.appendChild(el('span', 'k-more', '+' + (working.length - 4)));
      if (!working.length) {           // nobody at work: the hero shows the crew asleep, and what waits
        row.classList.add('asleep');
        all.idle.slice(0, 6).forEach(function (r) {
          var f = el('div', 'k-sleeper'); f.title = C.label(r.name) + ': ' + r.does;
          f.appendChild(C.booth({ id: r.id, kind: r.kind, name: r.name, state: 'idle' })); f.appendChild(el('span', null, C.label(r.name)));
          row.appendChild(f);
        });
        row.appendChild(el('p', 'k-asleep-line', 'Everyone is asleep' + (ready.length ? ' — ' + ready.length + ' stage' + (ready.length > 1 ? 's wait' : ' waits') + ' for you.' : '.')));
      }
    }

    // the rooms: worked-in rooms large, the rest one line each, the ones that wait on the owner first
    var busy = open.filter(function (l) { return l.workers.length; });
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
      if (!rooms.children.length) rooms.appendChild(el('p', 'k-empty', 'Nobody is at work right now. Every frog is in the lounge.'));
    }
    var lines = $('rooms-quiet');
    var cap = onBranches ? still.length : 3;      // the overview shows the first few; the branches page all
    if (changed(lines, JSON.stringify([still, cap]))) {
      still.slice(0, cap).forEach(function (l) { lines.appendChild(quietRoom(l)); });
      if (still.length > cap) { var more = el('a', 'k-room k-room-line k-room-more'); more.href = 'floor.html'; more.appendChild(el('b', null, 'All ' + (o.lanes.length) + ' branches →')); more.appendChild(el('span', 'k-waits', (still.length - cap) + ' more open, ' + (o.lanes.length - open.length) + ' quiet')); lines.appendChild(more); }
    }

    // the lounge: a strip of sleeping frogs, what each is for on hover
    var naps = $('naps');
    if (changed(naps, all.idle.map(function (r) { return r.id; }).join(','))) {
      all.idle.forEach(function (r) {
        var n = el('div', 'k-nap'); n.title = C.label(r.name) + ': ' + r.does;
        n.appendChild(C.booth({ id: r.id, kind: r.kind, name: r.name, state: 'idle', what: r.does }, 'k-round'));
        n.appendChild(el('b', null, C.label(r.name)));
        naps.appendChild(n);
      });
    }
    set('n-lounge', all.idle.length + ' asleep');

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
    if (window.drawBranches) window.drawBranches(o);
  }
  C.live(draw);
})();
