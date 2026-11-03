/* The Health page (health.html): window.HEALTH from data/health.js (src_health.py) as a card a subject.
   healthCards(H) is pure (test_health_page.py runs it under node); draw() puts the cards in #h-board. */
(function (root) {
  function txt(v) { return v === null || v === undefined || v === '' ? '-' : String(v); }

  function healthCards(H) {
    if (!H) return [{ title: 'No reading', tone: 'bad', lines: ['data/health.js is missing: src_health.py has not written it'] }];
    var cards = [];
    var p = H.paused || {};
    if (p.loop || p.supervisor) {
      cards.push({ title: 'Paused', tone: 'you', lines: [(p.loop ? 'autoloop.stop is set: the loop and its modules stand by. ' : '') +
        (p.supervisor ? 'supervisor.stop is set: no module takes a turn.' : '')] });
    }
    var b = H.breaker;
    cards.push(!b ? { title: 'Breaker', tone: '', lines: ['no record'] }
      : b.open ? { title: 'Breaker OPEN (' + txt(b.kind) + ')', tone: 'bad', lines: ['since ' + txt(b.since) + ': ' + txt(b.reason)].concat(b.evidence || []) }
      : { title: 'Breaker closed', tone: 'ok', lines: [txt(b.reason)] });
    var d = H.budget, c = H.capacity, bl = [];
    bl.push(d ? 'day: ' + txt(d.pct) + '% (' + txt(d.kind) + ')' + (d.end ? ' from ' + d.set_at + ' until ' + d.end : '') : 'day: no reading');
    bl.push(c ? 'relays allowed: ' + txt(c.allowed) + ' (' + txt(c.why) + ')' : 'capacity: no record');
    cards.push({ title: 'Budget and capacity', tone: c && c.allowed === 0 ? 'you' : '', lines: bl });
    var f = H.families;
    cards.push(!f ? { title: 'Families', tone: '', lines: ['no record'] }
      : { title: 'Families over cap: ' + f.over.length, tone: f.over.length ? 'you' : 'ok',
          lines: f.over.length ? f.over.map(function (x) { return x.family + ': ' + txt(x.over); }) : ['none (cap $' + txt(f.cap_usd) + ' or ' + txt(f.cap_generations) + ' generations)'] });
    var k = H.critic;
    cards.push(!k ? { title: 'Critic', tone: '', lines: ['no record'] }
      : { title: 'Critic holds: ' + k.held.length, tone: k.held.length ? 'you' : 'ok',
          lines: k.held.length ? k.held.map(function (x) { return x.unit + ': ' + txt(x.score) + ' (' + txt(x.at) + ')'; }) : ['none held; ' + k.passed + ' passed'] });
    var l = H.landing || {};
    cards.push({ title: 'Landing: ' + txt(l.phase), tone: '', lines: [(l.landed || {}).line || 'Landed this week: -'] });
    var v = H.canary;
    cards.push(!v ? { title: 'Canary', tone: '', lines: ['no record'] }
      : { title: 'Canary ' + txt(v.verdict), tone: v.verdict === 'green' ? 'ok' : v.verdict === 'red' ? 'bad' : '', lines: [txt(v.at) + ' ' + txt(v.line)] });
    cards.push({ title: 'Modules', tone: '', lines: (H.modules || []).map(function (m) {
      return m.name + ': ' + m.mode + (m.set ? '' : ' (default)') + ', last turn ' + (m.last || 'never'); }) });
    return cards;
  }

  function draw() {
    var H = root.HEALTH, board = document.getElementById('h-board'), count = document.getElementById('h-count');
    if (!board) return;
    var cards = healthCards(H);
    board.textContent = '';
    cards.forEach(function (c) {
      var el = document.createElement('article');
      el.className = 'r-card' + (c.tone ? ' h-' + c.tone : '');
      var h = document.createElement('h3');
      h.textContent = c.title;
      el.appendChild(h);
      c.lines.forEach(function (t) { var p = document.createElement('p'); p.textContent = t; el.appendChild(p); });
      board.appendChild(el);
    });
    if (count) count.textContent = H ? 'read ' + new Date(H.read_at * 1000).toLocaleString() : 'no reading';
  }

  root.healthCards = healthCards;
  if (typeof module !== 'undefined' && module.exports) module.exports = { healthCards: healthCards };
  if (typeof document !== 'undefined') {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', draw); else draw();
  }
})(typeof window !== 'undefined' ? window : globalThis);
