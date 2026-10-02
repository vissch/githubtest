// The floor (Tools/assetboard/ops.py writes data/ops.js; this draws it and reloads it every 20 seconds).
// A branch is a card: who is at work on it, its board items and their stages, the assets it touches, its last
// commits. A skill or agent with nothing to do sits on the bench; at work, it stands on the branch, moving.
(function () {
  var GLYPH = {   // a small mark per kind of worker, 24x24 strokes
    session: 'M5 6h14v9H9l-4 4z',
    machine: 'M4 7h16v10H4zM8 17v3M16 17v3M7 10h4M7 13h7',
    agent: 'M12 3l8 4.5v9L12 21l-8-4.5v-9zM12 8v8M8 12h8',
    role: 'M12 4a4 4 0 110 8 4 4 0 010-8zM5 20c1-4 4-6 7-6s6 2 7 6',
    'tw-vehicle-sim': 'M3 15h18M5 15l2-5h10l2 5M8 10V7h6v3M15 8h5M6 18a1.5 1.5 0 100-.1M18 18a1.5 1.5 0 100-.1',
    'tw-character-sim': 'M6 11c0-4 3-6 6-6s6 2 6 6zM4 11h16M9 15l3 6 3-6',
    'tw-critic': 'M2 12s4-7 10-7 10 7 10 7-4 7-10 7S2 12 2 12zM12 9a3 3 0 110 6 3 3 0 010-6',
    'tw-master': 'M6 4v16M6 8c6 0 6 8 12 8M18 4v4',
    'tw-balance-sim': 'M12 4v16M6 20h12M4 8h16M4 8l-2 6h4zM20 8l-2 6h4z',
    'tw-bug-catcher': 'M8 8h8v9a4 4 0 01-8 0zM9 8a3 3 0 016 0M4 12h4M16 12h4M5 18l3-2M19 18l-3-2',
    'tw-env-sim': 'M2 19l7-11 4 6 3-4 6 9z',
    'tw-optimizer': 'M4 17a8 8 0 1116 0M12 17l4-6',
    'tw-destruction-vfx': 'M12 2l2 6 6-2-4 5 5 4-6 1 1 6-4-4-4 4 1-6-6-1 5-4-4-5 6 2z',
    'tw-vfx-sheets': 'M3 6h18v12H3zM3 9h18M3 15h18M7 6v3M11 6v3M15 6v3M7 15v3M11 15v3M15 15v3',
    pipeline: 'M3 7h12l-3-3M21 17H9l3 3',
    'unity-pipeline': 'M12 3l8 4.5v9L12 21l-8-4.5v-9zM4 7.5l8 4.5 8-4.5M12 12v9'
  };
  function hue(s) { var h = 0; for (var i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360; return h; }
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function label(name) { return name.replace(/^tw-/, '').replace(/^agent:/, '').replace(/-/g, ' '); }
  function token(w, size) {
    var t = el('div', 'tok k-' + w.kind + ' ' + (w.state || 'idle'));
    var key = GLYPH[w.id] ? w.id : w.kind;
    t.style.setProperty('--h', hue(w.id || w.name));
    t.innerHTML = '<svg viewBox="0 0 24 24" width="' + (size || 26) + '" height="' + (size || 26) + '" aria-hidden="true"><path d="' + GLYPH[key] + '"/></svg>';
    t.title = (w.name || '') + (w.what ? ': ' + w.what : '');
    return t;
  }
  function ago(sec) { return sec < 90 ? 'now' : sec < 3600 ? Math.round(sec / 60) + ' min ago' : Math.round(sec / 3600) + ' h ago'; }

  function worker(w) {
    var box = el('div', 'worker ' + w.state);
    box.appendChild(token(w, 30));
    var say = el('div', 'say');
    say.appendChild(el('b', null, w.kind === 'session' ? (w.title || 'Claude session') : label(w.name)));
    if (w.kind === 'session') say.appendChild(el('span', 'tag', w.state === 'working' ? 'Claude · at work' : 'Claude · ' + ago(w.age)));
    else say.appendChild(el('span', 'tag', w.kind));
    if (w.what) say.appendChild(el('p', null, w.what));
    if (w.doing) say.appendChild(el('p', 'doing', '▸ ' + w.doing));
    box.appendChild(say);
    return box;
  }

  function lane(l) {
    var c = el('article', 'lane-card' + (l.workers.some(function (w) { return w.state === 'working'; }) ? ' hot' : ''));
    var h = el('header');
    h.appendChild(el('h3', null, l.branch));
    var meta = el('div', 'meta');
    if (l.checkout) meta.appendChild(el('span', 'tag', l.checkout));
    if (l.ahead) meta.appendChild(el('span', 'tag', l.ahead + ' ahead'));
    if (l.tip) meta.appendChild(el('span', 'tag', 'tip ' + l.tip));
    if (l.dirty) { var d = el('span', 'tag dirty', l.dirty + ' changing'); d.title = l.dirty_files.join('\n'); meta.appendChild(d); }
    h.appendChild(meta);
    c.appendChild(h);
    if (l.workers.length) { var ws = el('div', 'workers'); l.workers.forEach(function (w) { ws.appendChild(worker(w)); }); c.appendChild(ws); }
    l.items.forEach(function (it) {
      var row = el('div', 'item');
      row.appendChild(el('b', null, it.title || it.id));
      var st = el('div', 'stages');
      it.stages.forEach(function (s) {
        var p = el('span', 'stage st-' + s.state);
        if (s.skill || s.role) p.appendChild(token({ id: s.skill || s.role, kind: s.skill ? 'skill' : 'role', name: s.skill || s.role, state: s.state === 'RUNNING' ? 'working' : 'idle' }, 14));
        p.appendChild(document.createTextNode(s.id));
        p.title = s.id + ': ' + s.state + ' · ' + (s.skill || s.role || 'no role') + ' · ' + s.station;
        st.appendChild(p);
      });
      row.appendChild(st);
      c.appendChild(row);
    });
    if (l.assets.length) {
      var as = el('div', 'assets');
      l.assets.slice(0, 8).forEach(function (a) {
        var x = el('a', 'asset'); x.href = 'a/' + a.id + '.html'; x.title = a.name + ' · ' + a.status;
        if (a.pic) { var i = el('img'); i.src = a.pic; i.loading = 'lazy'; x.appendChild(i); }
        x.appendChild(el('span', null, a.name));
        as.appendChild(x);
      });
      c.appendChild(as);
    }
    if (l.last.length) {
      var ul = el('ul', 'last');
      l.last.slice(0, 2).forEach(function (k) { ul.appendChild(el('li', null, k.date + '  ' + k.subject)); });
      c.appendChild(ul);
    }
    return c;
  }

  function draw() {
    var o = window.OPS; if (!o) return;
    document.getElementById('stamp').textContent = 'read ' + window.OPS_NOW + ' on this machine · refreshes every 20 s while ops.py --watch runs';
    var counts = document.getElementById('counts'); counts.innerHTML = '';
    [['sessions', 'Claude sessions at work'], ['machines', 'machines running'], ['ready', 'stages ready to take'], ['idle', 'skills and agents idle']].forEach(function (p) {
      var t = el('div', 'tile'); t.appendChild(el('span', 'n', o.counts[p[0]])); t.appendChild(el('span', 'l', p[1])); counts.appendChild(t);
    });
    var busy = o.lanes.filter(function (l) { return l.workers.length || l.items.length || l.dirty; });
    var quiet = o.lanes.filter(function (l) { return busy.indexOf(l) < 0; });
    var b = document.getElementById('busy'); b.innerHTML = ''; busy.forEach(function (l) { b.appendChild(lane(l)); });
    document.getElementById('n-busy').textContent = busy.length + ' branches';
    var bench = document.getElementById('bench'); bench.innerHTML = '';
    var idle = o.roster.filter(function (r) { return !r.busy.length; });
    idle.forEach(function (r) {
      var s = el('div', 'seat'); s.appendChild(token({ id: r.id, kind: r.kind, name: r.name, state: 'idle' }, 30));
      var t = el('div'); t.appendChild(el('b', null, label(r.name))); t.appendChild(el('span', 'tag', r.kind + (r.where ? ' · ' + r.where : ''))); t.appendChild(el('p', null, r.does));
      s.appendChild(t); bench.appendChild(s);
    });
    document.getElementById('n-bench').textContent = idle.length + ' of ' + o.roster.length + ' idle';
    var q = document.getElementById('quiet'); q.innerHTML = '';
    quiet.forEach(function (l) {
      var r = el('div', 'qrow'); r.appendChild(el('b', null, l.branch));
      r.appendChild(el('span', 'dim', (l.tip ? 'tip ' + l.tip : '') + (l.ahead ? ' · ' + l.ahead + ' ahead' : '') + (l.live ? '' : ' · parked')));
      if (l.last[0]) r.appendChild(el('span', 'small', l.last[0].subject));
      q.appendChild(r);
    });
    document.getElementById('n-quiet').textContent = quiet.length;
  }
  draw();
  setInterval(function () {      // a fresh copy of the data: a new script tag, the old one dropped
    var s = document.createElement('script');
    s.src = 'data/ops.js?t=' + Date.now();
    s.onload = function () { draw(); s.remove(); };
    s.onerror = function () { s.remove(); };
    document.body.appendChild(s);
  }, 20000);
})();
