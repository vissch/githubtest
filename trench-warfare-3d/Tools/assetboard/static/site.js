// The overview's filters: kind, level and a name search. Cards carry data-cat, data-levels and data-name;
// a section with no card left says so, and the counts follow.
(function () {
  var state = { cat: '', level: '', q: '' };
  function apply() {
    document.querySelectorAll('.bucket').forEach(function (bucket) {
      var shown = 0;
      bucket.querySelectorAll('.card').forEach(function (card) {
        var ok = (!state.cat || card.dataset.cat === state.cat) &&
                 (!state.level || (' ' + card.dataset.levels + ' ').indexOf(' ' + state.level + ' ') >= 0) &&
                 (!state.q || card.dataset.name.indexOf(state.q) >= 0);
        card.hidden = !ok;
        document.body.classList.toggle('filtering', !!(state.cat || state.level || state.q));
        if (ok) shown++;
      });
      bucket.querySelector('.none').hidden = shown > 0;
      var more = bucket.querySelector('.k-showall');
      if (more) more.hidden = !!(state.cat || state.level || state.q) || bucket.classList.contains('open') || shown <= 10;
      document.querySelectorAll('[data-count="' + bucket.id + '"]').forEach(function (n) { n.textContent = shown; });
    });
  }
  document.querySelectorAll('.chips[data-filter]').forEach(function (group) {
    group.querySelectorAll('.chip').forEach(function (chip) {
      chip.addEventListener('click', function () {
        group.querySelectorAll('.chip').forEach(function (c) { c.classList.remove('on'); });
        chip.classList.add('on');
        state[group.dataset.filter] = chip.dataset.v;
        apply();
      });
    });
  });
  document.querySelectorAll('.bucket').forEach(function (bucket) {
    var cards = bucket.querySelectorAll('.card');
    cards.forEach(function (c, i) { if (i >= 10) c.classList.add('k-over'); });
    if (cards.length <= 10) return;
    var b = document.createElement('button'); b.className = 'k-showall'; b.type = 'button';
    b.textContent = 'Show all ' + cards.length + ' →';
    b.addEventListener('click', function () { bucket.classList.add('open'); b.hidden = true; });
    bucket.querySelector('.grid').after(b);
  });
  var q = document.getElementById('q');
  if (q) q.addEventListener('input', function () { state.q = q.value.trim().toLowerCase(); apply(); });
})();

// Cards whose first picture is the same (units the game draws with one shared figure) say so, so the twins read as
// intended rather than broken.
(function () {
  var seen = {};
  document.querySelectorAll('.card .pic video[poster], .card .pic > img').forEach(function (m) {
    var k = m.getAttribute('poster') || m.getAttribute('src'); (seen[k] = seen[k] || []).push(m.closest('.card'));
  });
  Object.keys(seen).forEach(function (k) {
    if (seen[k].length < 2) return;
    seen[k].forEach(function (c) { var t = c.querySelector('.tags'); if (!t) return; var s = document.createElement('span'); s.className = 'tag shared'; s.textContent = 'shared figure ×' + seen[k].length; t.appendChild(s); });
  });
})();

// A card's turntable plays while the pointer is on it; nothing is fetched before that.
document.querySelectorAll('.card video[data-src]').forEach(function (v) {
  var card = v.closest('.card');
  card.addEventListener('mouseenter', function () { if (!v.src) v.src = v.dataset.src; v.play().catch(function () {}); });
  card.addEventListener('mouseleave', function () { v.pause(); });
});
// An asset's films: the strip picks what the screen plays. Left and right step through them.
(function () {
  var screen = document.getElementById('screen');
  if (!screen) return;
  var clips = Array.prototype.slice.call(document.querySelectorAll('.clip'));
  function show(c) {
    clips.forEach(function (x) { x.classList.toggle('on', x === c); });
    screen.poster = c.dataset.poster; screen.src = c.dataset.src; screen.play().catch(function () {});
    document.getElementById('now-title').textContent = c.dataset.title;
    document.getElementById('now-src').textContent = c.dataset.what;
    c.scrollIntoView({ block: 'nearest' });
  }
  clips.forEach(function (c) { c.addEventListener('click', function () { show(c); }); });
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') return;
    if (/INPUT|TEXTAREA/.test((e.target || {}).tagName || '')) return;
    var k = clips.findIndex(function (x) { return x.classList.contains('on'); });
    var n = clips[(k + (e.key === 'ArrowRight' ? 1 : clips.length - 1)) % clips.length];
    if (n) { e.preventDefault(); show(n); }
  });
})();
