// The crew on the overview: who is at work (a frog per session, machine, skill and agent, with what it is doing and
// on which branch), who sleeps, and on each card the frogs at work on a branch that touches that asset. Reads
// data/ops.js (ops.py writes it; --watch keeps it fresh) and redraws every 20 seconds. Drawn by crew.js.
(function () {
  var C = window.Crew, el = C.el;
  var pond = document.getElementById('crew');
  if (!pond || !C) return;

  function draw() {
    var o = window.OPS;
    if (!o) { pond.hidden = true; return; }
    pond.hidden = false;
    var all = C.everyone(o);
    var working = all.at.filter(function (x) { return x.w.state === 'working'; });
    var resting = all.at.filter(function (x) { return x.w.state !== 'working'; });

    var at = document.getElementById('crew-at'); at.innerHTML = '';
    working.concat(resting).forEach(function (x) {
      var w = x.w, box = el('div', 'mate ' + (w.state || 'idle'));
      box.appendChild(C.frog(w, 72));
      var say = el('div', 'say');
      say.appendChild(el('b', null, w.kind === 'session' ? (w.title || 'Claude session') : C.label(w.name)));
      say.appendChild(el('span', 'tag', w.kind === 'session' ? (w.state === 'working' ? 'Claude · at work' : 'Claude · ' + C.ago(w.age || 0)) : w.kind));
      say.appendChild(el('p', 'where', '⎇ ' + x.where));
      if (w.doing) say.appendChild(el('p', 'doing', '▸ ' + w.doing));
      if (w.what) say.appendChild(el('p', 'what', w.what));
      box.appendChild(say);
      at.appendChild(box);
    });
    if (!all.at.length) at.appendChild(el('p', 'dim', 'Nobody is at work right now.'));

    var sleep = document.getElementById('crew-sleep'); sleep.innerHTML = '';
    all.idle.forEach(function (r) {
      var s = el('div', 'nap');
      s.appendChild(C.frog({ id: r.id, kind: r.kind, name: r.name, state: 'idle', what: r.does }, 54));
      s.appendChild(el('span', null, C.label(r.name)));
      sleep.appendChild(s);
    });
    document.getElementById('n-crew').textContent = working.length + ' at work · ' + all.idle.length + ' asleep';
    document.getElementById('crew-stamp').textContent = 'read ' + window.OPS_NOW;

    // the cards: frogs at work on a branch that touches the asset
    var on = {};   // asset id -> [{w, where}]
    var assetsOf = {};
    o.lanes.forEach(function (l) { assetsOf[l.branch] = l.assets.map(function (a) { return a.id; }); });
    all.at.forEach(function (x) {
      x.branches.forEach(function (b) {
        (assetsOf[b] || []).forEach(function (id) { (on[id] = on[id] || []).push(x); });
      });
    });
    document.querySelectorAll('.card .on-it').forEach(function (spot) {
      spot.innerHTML = '';
      var who = on[spot.dataset.asset] || [];
      spot.closest('.card').classList.toggle('worked', who.some(function (x) { return x.w.state === 'working'; }));
      who.slice(0, 4).forEach(function (x) {
        var f = C.frog(x.w, 38);
        f.title += ' · on ' + x.where;
        spot.appendChild(f);
      });
      if (who.length > 4) spot.appendChild(el('span', 'more', '+' + (who.length - 4)));
    });
  }
  C.live(draw);
})();
