// The owner queue on the page: data/queue.js (src_queue.py) as rows of chips, and how old the page's data is
// (data/beat.js, written on every read). Pure functions, no DOM, so test_assetboard.py runs them under node: the
// tile's number is the number of his rows listed, a row of his leads to its brief and never to a branch, and a page
// nobody has read for an hour says so.
(function (root) {
  // What waits on the owner (YOURS: the rows and the number of "Needs you") and what waits on an agent (AGENTS: listed
  // apart, never counted as his). src_queue.py says which is which and why.
  var YOURS = [
    { key: 'briefs', label: 'Decide' },
    { key: 'decide', label: 'Questions, no brief yet' }
  ];
  var AGENTS = [
    { key: 'broken', label: 'Broken' },
    { key: 'said', label: 'You said land' },
    { key: 'approved', label: 'Approved, not landed' },
    { key: 'owed', label: 'Owe you a brief' },
    { key: 'ready', label: 'Ready for an agent' }
  ];
  var STALE_MINUTES = 60;
  function short(b) { return (b || '').replace(/^lane\/(show|sim)\//, ''); }
  function age(days) { return days === null || days === undefined ? '' : days <= 0 ? 'today' : days === 1 ? '1 day' : days + ' days'; }
  function chips(list) { return list.filter(function (c) { return c; }); }
  function n(k, one, many) { return k ? (k === 1 ? one : k + ' ' + many) : ''; }
  // one row per entry: what it is, the chips under it, the words on hover. A row of his names its brief (`brief`), or
  // is a question with none; a row of the agents' never leads anywhere.
  function row(g, e) {
    if (g === 'briefs') return { top: e.title, brief: e.brief, lane: e.lane, shot: e.shot || '', pick: e.pick || '', tip: e.what_for || e.title,
                                 chips: chips([age(e.days), e.kind === 'concepts' ? 'concepts to pick from' : n(e.options, '1 option', 'options'), n(e.films, 'a film', 'films'), n(e.stills, '1 picture', 'pictures')]) };
    if (g === 'decide') return { top: e.title, chips: chips([age(e.days), e.choice ? 'agent chose' : '', 'no brief yet']), tip: e.text };
    if (g === 'said') return { top: short(e.lane), lane: e.lane, tip: e.lane + ': you said land, the rest is an agent\'s',
                               chips: chips(['you said “' + e.words + '”', age(e.days), e.behind ? 'to rebase and gate again' : 'to land']) };
    if (g === 'owed') return { top: short(e.lane), lane: e.lane, tip: e.lane + ': its gate is green and nobody has written its brief',
                               chips: chips(['gate green', 'no brief for you yet', e.behind ? e.behind + ' behind' : '']) };
    if (g === 'approved') return { top: short(e.lane), lane: e.lane, tip: e.lane + ': the owner said land on ' + e.date,
                                   chips: chips(['“' + e.words + '”', age(e.days)].concat(e.why || [])) };
    if (g === 'ready') return { top: e.title, lane: e.lane, tip: e.item + ': ' + e.stage + ' is ready' + (e.role ? ' (' + e.role + ')' : ''),
                                chips: chips([e.stage, short(e.lane), age(e.days)]) };
    return { top: e.title, lane: e.lane, url: e.url, tip: e.text || e.title, chips: chips([short(e.lane), age(e.days)]) };
  }
  // what a click on a row of the agents' opens (board.js): the few lines src_queue.py details() wrote, the pictures that
  // show it, and what he can say should happen. A click on one of those leaves a note in his words: it is his word, as a
  // typed one is. A row of his opens its brief instead, which has all of that.
  var SAY = {
    approved: [['Land it now', 'Land it now.'], ['Hold it', 'Hold it: do not land it yet.']],
    ready: [['Take it next', 'Take this step next.'], ['Leave it', 'Leave this step for now.']],
    stranded: [['Land this lane', 'Land this lane, so its decisions reach the others.']],
    untaken: [['Take them up now', 'Take my answers up now.']]
  };
  function more(g, e) {
    var acts = SAY[g === 'broken' ? e.kind : g] || [];
    return { detail: (e.detail || []).slice(0, 5), shots: (e.shots || []).slice(0, 3), actions: acts.map(function (a) { return { label: a[0], say: a[1] }; }) };
  }
  // the groups that hold something, in the order they are listed
  function list(defs, q) {
    return defs.map(function (g) {
      return { key: g.key, label: g.label, rows: ((q && q[g.key]) || []).map(function (e) { return row(g.key, e); }),
               more: ((q && q[g.key]) || []).map(function (e) { return more(g.key, e); }) };
    }).filter(function (g) { return g.rows.length; });
  }
  function groups(q) { return list(YOURS, q); }          // his
  function agents(q) { return list(AGENTS, q); }         // not his
  function total(gs) { return gs.reduce(function (k, g) { return k + g.rows.length; }, 0); }
  function count(q) { return total(groups(q)); }
  // where a row of his leads: its brief on the Decide page, or its place among the questions without one. Nothing else:
  // a row never leads to a branch (the owner, 2026-10-06: "these only click me through towards the branches")
  function slug(s) { return String(s || '').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 48); }
  function leads(r) { return 'decide.html#' + (r.brief || 'q-' + slug(r.top)); }
  // how long ago the floor was last read; stale past an hour (the watcher has stopped, or the Drive has not synced)
  function fresh(beat, nowMs) {
    var t = beat ? new Date(beat).getTime() : NaN;
    if (isNaN(t)) return { minutes: null, stale: false, text: '' };
    var m = Math.max(0, Math.round((nowMs - t) / 60000));
    return { minutes: m, stale: m > STALE_MINUTES,
             text: m < 1 ? 'read just now' : m < 120 ? 'read ' + m + ' min ago' : m < 2880 ? 'read ' + Math.round(m / 60) + ' h ago' : 'read ' + Math.round(m / 1440) + ' days ago' };
  }
  var api = { groups: groups, agents: agents, count: count, total: total, leads: leads, fresh: fresh, short: short, more: more, STALE_MINUTES: STALE_MINUTES };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.OwnerQueue = api;
})(typeof window !== 'undefined' ? window : this);
