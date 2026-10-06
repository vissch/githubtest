// The control screen's own part (index.html, control.css). What it says: the headline is the answer (how many are
// at work, how much waits on the owner), and beside the time of the last reading how long ago that was, counted
// every second, so a watcher that fell behind shows within a minute and not after the hour it takes to turn red.
// How it moves: a tile's number counts to its value, on the first reading and whenever a reading changes it, and a
// graph grows from its baseline the first time it is on the screen; none of that when the reader asked for less
// motion. The parts draw themselves (office.js, housedraw.js, charts.js, board.js); this only watches what they
// wrote. The words and the steps of a count are plain functions (test_assetboard.py runs them under node).
(function (root) {
  'use strict';
  // a number as the tiles write it (4,028) and back
  function plain(text) { return /^\d[\d,]*$/.test(text) ? +text.replace(/,/g, '') : null; }
  function grouped(n) { return String(Math.round(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ','); }
  // where a count from `a` to `b` stands at the part `k` (0 to 1) of its time: fast at first, settling on b exactly
  function step(a, b, k) { k = Math.max(0, Math.min(1, k)); return k >= 1 ? b : Math.round(a + (b - a) * (1 - Math.pow(1 - k, 3))); }
  // the headline, from the two numbers the tiles show: [who is at work, what waits]
  function headline(at, needs) {
    return [at ? grouped(at) + ' at work,' : 'Nobody at work,', needs ? grouped(needs) + (needs === 1 ? ' waits on you.' : ' wait on you.') : 'nothing waits on you.'];
  }
  // how old the last reading is, in words, and whether it is late: a reading comes every 20 s, so after LATE
  // seconds three were missed
  var LATE = 60;
  function age(sec) {
    sec = Math.max(0, Math.round(sec));
    var words = sec < 60 ? sec + ' s ago' : sec < 3600 ? Math.round(sec / 60) + ' min ago' : Math.round(sec / 3600) + ' h ago';
    return { words: sec > LATE ? words + ', late' : words, late: sec > LATE };
  }
  var pure = { plain: plain, grouped: grouped, step: step, headline: headline, age: age, LATE: LATE };
  if (typeof module !== 'undefined' && module.exports) { module.exports = pure; return; }

  if (!document.body.classList.contains('c-screen')) return;
  var calm = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  function $(id) { return document.getElementById(id); }

  // ---- the headline and the age of the reading
  // the headline is said once the tiles' numbers stand still (they count up to their value): a screen reader hears
  // the answer once, not every step of the count
  var settle = 0;
  function say() { clearTimeout(settle); settle = setTimeout(said, calm ? 0 : 800); }
  function said() {
    var at = plain($('p-at') ? $('p-at').textContent : ''), needs = plain($('p-ready') ? $('p-ready').textContent : '');
    if (at === null || needs === null || !$('c-h-at')) return;
    var h = headline(at, needs);
    if ($('c-h-at').textContent !== h[0]) $('c-h-at').textContent = h[0];
    if ($('c-h-needs').textContent !== h[1]) $('c-h-needs').textContent = h[1];
  }
  function old() {
    var e = $('c-age'), beat = window.BEAT || (window.OPS_NOW || '').replace(' ', 'T'), t = beat ? new Date(beat).getTime() : NaN;
    if (!e || isNaN(t)) return;
    var a = age((Date.now() - t) / 1000);
    e.textContent = ' · ' + a.words; e.parentNode.classList.toggle('late', a.late);
  }
  if ('MutationObserver' in window) ['p-at', 'p-ready'].forEach(function (id) { if ($(id)) new MutationObserver(say).observe($(id), { childList: true, characterData: true, subtree: true }); });
  say(); old(); setInterval(old, 1000);

  if (calm || !('MutationObserver' in window)) { document.querySelectorAll('.c-graphs .g-card').forEach(function (c) { c.classList.add('c-seen'); }); return; }

  // ---- the numbers on the tiles
  var TIME = 700;
  function count(e) {
    var shown = 0, goal = null, run = 0, wrote = null;         // wrote: the observer hears this script's own steps too, later
    function write(n, comma) { wrote = comma ? grouped(n) : String(n); e.textContent = wrote; }
    function to(text) {
      var n = plain(text); if (n === null || n === goal) return;
      var from = goal === null ? 0 : shown, comma = text.indexOf(',') >= 0 || n >= 1000 && e.id === 't-calls', t0 = performance.now(), id = ++run;
      goal = n;
      (function tick(now) {
        if (id !== run) return;
        shown = step(from, n, (now - t0) / TIME); write(shown, comma);
        if (shown !== n) requestAnimationFrame(tick);
      })(t0);
    }
    new MutationObserver(function () { if (e.textContent !== wrote) to(e.textContent); }).observe(e, { childList: true, characterData: true, subtree: true });
    to(e.textContent);
  }
  ['p-ready', 'p-at', 'p-rooms', 't-calls', 't-notes'].forEach(function (id) { var e = document.getElementById(id); if (e) count(e); });

  // ---- the graphs: a card is seen when a third of it is on the screen; its columns then rise one after the other
  var cards = document.querySelectorAll('.c-graphs .g-card');
  function seen(c) {
    Array.prototype.forEach.call(c.querySelectorAll('.g-plot svg > g'), function (g, i, all) { g.style.transitionDelay = Math.round(i * Math.min(14, 420 / all.length)) + 'ms'; });
    c.classList.add('c-seen');
    setTimeout(function () { Array.prototype.forEach.call(c.querySelectorAll('.g-plot svg > g'), function (g) { g.style.transitionDelay = ''; }); }, 1400);
  }
  if ('IntersectionObserver' in window) {
    var io = new IntersectionObserver(function (es) { es.forEach(function (x) { if (x.isIntersecting) { io.unobserve(x.target); seen(x.target); } }); }, { threshold: 0.3 });
    Array.prototype.forEach.call(cards, function (c) { io.observe(c); });
  } else Array.prototype.forEach.call(cards, seen);
})(typeof window !== 'undefined' ? window : this);
