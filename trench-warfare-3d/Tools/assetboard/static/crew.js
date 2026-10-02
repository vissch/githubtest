// The crew: every Claude session, machine, skill and agent drawn as a frog (img/crew-head.png; ops.py copies it in
// when the station has one, and without it the frog falls back to a mark). Each one is tinted by its name and holds
// the mark of its trade; at work it bobs, idle it sleeps. Used by the overview (index.html) and the floor.
window.Crew = (function () {
  var GLYPH = {   // a small mark per kind of worker, 24x24 strokes
    session: 'M5 6h14v9H9l-4 4z',
    machine: 'M4 7h16v10H4zM8 17v3M16 17v3M7 10h4M7 13h7',
    agent: 'M12 3l8 4.5v9L12 21l-8-4.5v-9zM12 8v8M8 12h8',
    skill: 'M12 4a4 4 0 110 8 4 4 0 010-8zM5 20c1-4 4-6 7-6s6 2 7 6',
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
  var noFrog = false;    // set once the picture fails to load: every later frog is drawn as its mark
  function hue(s) { var h = 0; for (var i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360; return h; }
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function label(name) { return (name || '').replace(/^tw-/, '').replace(/^agent:/, '').replace(/-/g, ' '); }
  function mark(key, size) {
    return '<svg viewBox="0 0 24 24" width="' + size + '" height="' + size + '" aria-hidden="true"><path d="' + (GLYPH[key] || GLYPH.skill) + '"/></svg>';
  }
  // a frog: w = {id, kind, name, state, what}; size in px
  function frog(w, size) {
    size = size || 56;
    var id = w.id || w.name || '';
    var f = el('div', 'frog k-' + w.kind + ' ' + (w.state || 'idle'));
    f.style.setProperty('--s', size + 'px');
    // sessions keep the frog's own green (they lead); the rest turn by their name, machines go steel
    f.style.setProperty('--turn', w.kind === 'session' ? '0deg' : hue(id) + 'deg');
    f.title = (w.kind === 'session' ? (w.title || 'Claude session') : label(w.name)) + (w.what ? ': ' + w.what : '');
    if (!noFrog) {
      var img = el('img'); img.alt = ''; img.src = 'img/crew-head.png';
      img.onerror = function () { noFrog = true; f.classList.add('bare'); img.remove(); };
      f.appendChild(img);
    } else f.classList.add('bare');
    var prop = el('span', 'prop'); prop.style.setProperty('--h', hue(id));
    prop.innerHTML = mark(GLYPH[id] ? id : w.kind, Math.round(size * 0.3));
    f.appendChild(prop);
    return f;
  }
  // a small mark only (for the stages of a board item)
  function token(w, size) {
    var t = el('div', 'tok k-' + w.kind + ' ' + (w.state || 'idle'));
    t.style.setProperty('--h', hue(w.id || w.name || ''));
    t.innerHTML = mark(GLYPH[w.id] ? w.id : w.kind, size || 26);
    t.title = (w.name || '') + (w.what ? ': ' + w.what : '');
    return t;
  }
  function ago(sec) { return sec < 90 ? 'now' : sec < 3600 ? Math.round(sec / 60) + ' min ago' : Math.round(sec / 3600) + ' h ago'; }
  // everyone, from a reading of the floor: who is at work where (sessions and machines from the branches, skills and
  // agents from the roster) and who is idle
  function everyone(o) {
    var at = [], seen = {};
    o.lanes.forEach(function (l) {
      l.workers.forEach(function (w) {
        var k = w.id + '@' + l.branch; if (seen[k]) return; seen[k] = 1;
        at.push({ w: w, where: l.branch, branches: [l.branch] });
      });
    });
    var idle = [];
    o.roster.forEach(function (r) {
      if (r.busy.length) {
        if (!at.some(function (x) { return x.w.id === r.id; }))
          at.push({ w: { id: r.id, kind: r.kind, name: r.name, state: 'working', what: r.does }, where: r.busy.join(', '), branches: r.busy });
      } else idle.push(r);
    });
    return { at: at, idle: idle };
  }
  // read the floor again every 20 s: a new script tag for data/ops.js, the old one dropped
  function live(draw) {
    draw();
    setInterval(function () {
      var s = document.createElement('script');
      s.src = 'data/ops.js?t=' + Date.now();
      s.onload = function () { draw(); s.remove(); };
      s.onerror = function () { s.remove(); };
      document.body.appendChild(s);
    }, 20000);
  }
  return { GLYPH: GLYPH, hue: hue, el: el, label: label, frog: frog, token: token, ago: ago, everyone: everyone, live: live };
})();
