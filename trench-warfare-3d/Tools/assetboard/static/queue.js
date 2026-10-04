// The owner queue on the page: data/queue.js (src_queue.py) as rows of chips, and how old the page's data is
// (data/beat.js, written on every read). Pure functions, no DOM, so test_assetboard.py runs them under node: the
// tile's number is the number of rows listed, and a page nobody has read for an hour says so.
(function (root) {
  var GROUPS = [
    { key: 'broken', label: 'Broken', verb: 'Fix' },
    { key: 'land', label: 'Say land', verb: 'Land' },
    { key: 'approved', label: 'Approved, not landed', verb: 'Chase' },
    { key: 'ready', label: 'Ready to take', verb: 'Take' },
    { key: 'decide', label: 'Decide', verb: 'Decide' }
  ];
  var STALE_MINUTES = 60;
  function short(b) { return (b || '').replace(/^lane\/(show|sim)\//, ''); }
  function age(days) { return days === null || days === undefined ? '' : days <= 0 ? 'today' : days === 1 ? '1 day' : days + ' days'; }
  function chips(list) { return list.filter(function (c) { return c; }); }
  // one row per entry: what it is, the chips under it, the words on hover
  function row(g, e) {
    if (g === 'decide') return { top: e.title, chips: chips([age(e.days), e.choice ? 'agent chose' : '']), tip: e.text };
    if (g === 'land') return { top: short(e.lane), lane: e.lane, tip: e.lane + ': the full gate went green on its tip',
                               chips: chips(['gate green', e.ahead + ' commits', e.behind ? e.behind + ' behind' : '']) };
    if (g === 'approved') return { top: short(e.lane), lane: e.lane, tip: e.lane + ': the owner said land on ' + e.date,
                                   chips: chips(['“' + e.words + '”', age(e.days)].concat(e.why || [])) };
    if (g === 'ready') return { top: e.title, lane: e.lane, tip: e.item + ': ' + e.stage + ' is ready' + (e.role ? ' (' + e.role + ')' : ''),
                                chips: chips([e.stage, short(e.lane), age(e.days)]) };
    return { top: e.title, lane: e.lane, url: e.url, tip: e.text || e.title, chips: chips([short(e.lane), age(e.days)]) };
  }
  // the groups that hold something, in the order they are listed
  function groups(q) {
    return GROUPS.map(function (g) {
      return { key: g.key, label: g.label, verb: g.verb, rows: ((q && q[g.key]) || []).map(function (e) { return row(g.key, e); }) };
    }).filter(function (g) { return g.rows.length; });
  }
  function count(q) { return groups(q).reduce(function (n, g) { return n + g.rows.length; }, 0); }
  // how long ago the floor was last read; stale past an hour (the watcher has stopped, or the Drive has not synced)
  function fresh(beat, nowMs) {
    var t = beat ? new Date(beat).getTime() : NaN;
    if (isNaN(t)) return { minutes: null, stale: false, text: '' };
    var m = Math.max(0, Math.round((nowMs - t) / 60000));
    return { minutes: m, stale: m > STALE_MINUTES,
             text: m < 1 ? 'read just now' : m < 120 ? 'read ' + m + ' min ago' : m < 2880 ? 'read ' + Math.round(m / 60) + ' h ago' : 'read ' + Math.round(m / 1440) + ' days ago' };
  }
  var api = { groups: groups, count: count, fresh: fresh, short: short, STALE_MINUTES: STALE_MINUTES };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.OwnerQueue = api;
})(typeof window !== 'undefined' ? window : this);
