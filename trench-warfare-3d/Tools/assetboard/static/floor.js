// The floor (Tools/assetboard/ops.py writes data/ops.js; this draws it and reloads it every 20 seconds).
// A branch is a card: who is at work on it, its board items and their stages, the assets it touches, its last
// commits. A skill or agent with nothing to do sits on the bench; at work, it stands on the branch, moving.
// The workers are drawn by crew.js.
(function () {
  var C = window.Crew, el = C.el;

  function worker(w) {
    var box = el('div', 'worker ' + w.state);
    box.appendChild(C.frog(w, 48));
    var say = el('div', 'say');
    say.appendChild(el('b', null, w.kind === 'session' ? (w.title || 'Claude session') : C.label(w.name)));
    if (w.kind === 'session') say.appendChild(el('span', 'tag', w.state === 'working' ? 'Claude · at work' : 'Claude · ' + C.ago(w.age)));
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
        if (s.skill || s.role) p.appendChild(C.token({ id: s.skill || s.role, kind: s.skill ? 'skill' : 'role', name: s.skill || s.role, state: s.state === 'RUNNING' ? 'working' : 'idle' }, 14));
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
      var s = el('div', 'seat'); s.appendChild(C.frog({ id: r.id, kind: r.kind, name: r.name, state: 'idle', what: r.does }, 48));
      var t = el('div'); t.appendChild(el('b', null, C.label(r.name))); t.appendChild(el('span', 'tag', r.kind + (r.where ? ' · ' + r.where : ''))); t.appendChild(el('p', null, r.does));
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
  C.live(draw);
})();
