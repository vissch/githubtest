// The crew: every Claude session, machine, skill and agent is a frog at a desk. Each has its own look (Krea2 stills)
// and two loops (Minimax H3): busy at work, and dozing. ops.py copies them into img/crew/ when the station has them
// and lists them in data/crew.js (window.CREW_MEDIA); without them a worker is drawn as the mark of its trade.
// Used by the overview (office.js) and the branches page (floor.js).
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
  var MEDIA = window.CREW_MEDIA || {};      // key -> {busy: bool, doze: bool}
  function hue(s) { var h = 0; for (var i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360; return h; }
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function label(name) { return (name || '').replace(/^tw-/, '').replace(/^agent:/, '').replace(/-/g, ' '); }
  function mark(key, size) {
    return '<svg viewBox="0 0 24 24" width="' + size + '" height="' + size + '" aria-hidden="true"><path d="' + (GLYPH[key] || GLYPH.skill) + '"/></svg>';
  }
  // which frog plays this worker
  function key(w) {
    if (w.kind === 'session' || w.kind === 'machine') return w.kind;
    var id = (w.id || w.name || '').replace(/^agent:/, '');
    if (MEDIA[id]) return id;
    return w.kind === 'agent' ? 'agent' : 'role';
  }
  function title(w) { return w.kind === 'session' ? (w.title || 'Claude session') : w.kind === 'machine' ? (w.name || 'machine') : label(w.name || w.id); }

  // videos play only while on screen, so a page of thirty frogs costs what the visible few cost
  var seen = 'IntersectionObserver' in window ? new IntersectionObserver(function (es) {
    es.forEach(function (e) { var v = e.target; if (e.isIntersecting) { v.play().catch(function () {}); } else v.pause(); });
  }, { rootMargin: '120px' }) : null;
  var still = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  // a booth: the frog's loop (or poster, or mark) in a rounded card; w = {id, kind, name, state, ...}
  function booth(w, cls) {
    var k = key(w), m = MEDIA[k], mode = w.state === 'working' ? 'busy' : 'doze';
    var b = el('div', 'k-booth ' + (w.state || 'idle') + ' kind-' + w.kind + (cls ? ' ' + cls : ''));
    b.style.setProperty('--h', hue(w.id || w.name || ''));
    if (m) {
      var v = el('video'); v.muted = true; v.loop = true; v.playsInline = true; v.setAttribute('playsinline', '');
      v.preload = 'none'; v.poster = 'img/crew/' + k + '.jpg';
      if (m[mode] && !still) { v.src = 'img/crew/' + k + '.' + mode + '.mp4'; if (seen) seen.observe(v); else v.autoplay = true; }
      b.appendChild(v);
    } else {
      var g = el('div', 'k-mark'); g.innerHTML = mark(GLYPH[w.id] ? w.id : w.kind, 34); b.appendChild(g);
    }
    var badge = el('span', 'k-badge'); badge.innerHTML = mark(GLYPH[w.id] ? w.id : w.kind, 14); b.appendChild(badge);
    b.title = title(w) + (w.what ? ': ' + w.what : '');
    return b;
  }
  // the old round avatar, now a small booth
  function frog(w, size) { var b = booth(w, 'k-mini'); b.style.width = b.style.height = (size || 56) + 'px'; return b; }
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
    var at = [], seenW = {};
    o.lanes.forEach(function (l) {
      l.workers.forEach(function (w) {
        var k = w.id + '@' + l.branch; if (seenW[k]) return; seenW[k] = 1;
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
  // the top bar's live pill, on every page that has the floor's data
  function nowPill(o) {
    var p = document.getElementById('k-now'); if (!p || !o) return;
    var all = everyone(o), n = all.at.filter(function (x) { return x.w.state === 'working'; }).length;
    document.getElementById('k-now-text').textContent = n + ' at work · ' + all.idle.length + ' asleep · ' + o.counts.ready + ' ready';
    p.hidden = false;
  }
  return { GLYPH: GLYPH, MEDIA: MEDIA, hue: hue, el: el, label: label, key: key, title: title, booth: booth, frog: frog,
           token: token, ago: ago, everyone: everyone, live: live, nowPill: nowPill };
})();
