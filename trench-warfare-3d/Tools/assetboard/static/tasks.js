// The tasks on the page: data/tasks.js (tasks.py) as groups of rows, what a click on one opens, and the two things
// the owner can say about one. Pure functions, no DOM, so test_assetboard.py runs them under node: a task he has
// queued or dropped shows so from his click, before the next reading says it too, and a capture is told by when it
// was taken, the rest by how long nobody has been on them.
(function (root) {
  // the kinds, in the order the page lists them (src_tasks.py KINDS), each with the words over its rows
  var KINDS = [
    { key: 'capture', label: 'Your feedback from the game' },
    { key: 'agent', label: 'Agents that were cut off' },
    { key: 'session', label: 'Sessions that stopped mid-work' },
    { key: 'handoff', label: 'Handoffs nobody picked up' },
    { key: 'unit', label: 'Written for the relay, never queued' },
    { key: 'relay', label: 'Tried by the relay, not finished' },
    { key: 'ready', label: 'Pipeline steps nobody took' }
  ];
  // what he can say about a task: a click leaves the second as a note of his (src_tasks.py QUEUE_SAY, DROP_SAY)
  var QUEUE = 'Queue this for the relay.', DROP = 'Not needed: drop this task.';
  var SAY = [{ label: 'Queue it for the relay', say: QUEUE }, { label: 'Not needed', say: DROP }];
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  function span(min) { min = Math.max(0, Math.round(min || 0)); return min < 1 ? 'under a minute' : min < 120 ? min + ' min' : min < 2880 ? Math.round(min / 60) + ' h' : Math.round(min / 1440) + ' days'; }
  function n(k, one, many) { return k ? (k === 1 ? one : k + ' ' + many) : ''; }
  // where a task stands once his own notes are counted: `said` are the texts of his open notes about it, which the
  // page knows from the click on, a reading or more before data/tasks.js does
  function state(r, said) {
    said = said || [];
    if (r.state !== 'left') return r.state;
    return said.indexOf(DROP) >= 0 ? 'dropped' : said.indexOf(QUEUE) >= 0 ? 'queued' : 'left';
  }
  function chips(r, st) {
    var c = [];
    if (st === 'queued') c.push('queued for the relay');
    c.push(r.kind === 'capture' ? 'captured ' + span(r.idle) + ' ago' : 'nobody on it for ' + span(r.idle));
    if (r.where) c.push('on ' + r.where);
    if (r.lane) c.push(short(r.lane));
    if (r.kind === 'handoff' && r.file) c.push(String(r.file).replace(/^HANDOFF_AGENT_|\.md$/g, ''));          // two handoffs can share a topic: the file tells them apart
    if (r.kind === 'agent' && r.agent) c.push(r.agent + ' agent');
    if (r.kind === 'relay' && r.verdict) c.push(String(r.verdict).toLowerCase());
    if (r.asks) c.push('asked you something');
    if (r.words && r.words.length) c.push(n(r.words.length, '1 note of yours', 'notes of yours'));
    return c;
  }
  // one row: what it is, a line under it, the chips, its picture when it has one
  function row(r, said) {
    var st = state(r, said);
    return { id: r.id, top: r.title, sub: r.kind === 'capture' ? ((r.detail || [])[0] || '') : (r.what || ''), shot: r.shots && r.shots[0] ? r.shots[0].src : '', chips: chips(r, st), state: st,
             tip: (r.detail || [])[0] || r.title };
  }
  // what a click on a row opens (board.js): the lines tasks.py wrote, the picture, and the two buttons while nothing
  // has been said. The subject is the task and nothing wider, so the notes under it are the notes about it.
  function subject(r, label, said) {
    var st = state(r, said);
    return { kind: 'queue', id: 'task: ' + r.id, kindLabel: 'task · ' + String(label || r.kind).toLowerCase(), title: r.title, facts: chips(r, st), detail: (r.detail || []).slice(0, 5),
             shots: (r.shots || []).slice(0, 3), wantsShots: r.kind === 'capture', actions: st === 'left' ? SAY : [] };
  }
  // the groups that hold something, in order; a task he dropped is not listed
  function groups(T, saidOf) {
    var rows = (T && T.rows) || [];
    return KINDS.map(function (k) {
      var mine = rows.filter(function (r) { return r.kind === k.key && state(r, saidOf ? saidOf(r) : []) !== 'dropped'; });
      return { key: k.key, label: k.label, raw: mine, rows: mine.map(function (r) { return row(r, saidOf ? saidOf(r) : []); }) };
    }).filter(function (g) { return g.rows.length; });
  }
  function count(gs, st) { return gs.reduce(function (k, g) { return k + g.rows.filter(function (r) { return r.state === st; }).length; }, 0); }
  // the line under the page's title: how many wait for an agent, how many he queued, how long the relay's queue is
  function head(T, gs) {
    if (!T) return 'The tasks have not been read yet.';
    var left = count(gs, 'left'), queued = count(gs, 'queued'), out = [];
    out.push(left ? n(left, '1 task waits for an agent', 'tasks wait for an agent') : 'Nothing is left unfinished');
    if (queued) out.push(n(queued, '1 queued for the relay', 'queued for the relay'));
    if (T.relay && T.relay.waiting) out.push(n(T.relay.waiting, '1 unit', 'units') + ' in the relay\'s queue' + (T.relay.as_of ? ' as of ' + T.relay.as_of : ''));
    return out.join(' · ');
  }
  // which stations the list was read on, and how long ago each
  function heard(T, nowSec) {
    return ((T && T.stations) || []).map(function (s) { var m = Math.max(0, (nowSec - s.at) / 60); return s.host + (m < 2 ? ' just now' : ' ' + span(m) + ' ago'); }).join(', ');
  }
  var api = { KINDS: KINDS, SAY: SAY, QUEUE: QUEUE, DROP: DROP, span: span, state: state, chips: chips, row: row, subject: subject, groups: groups, count: count, head: head, heard: heard, short: short };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.Tasks = api;
})(typeof window !== 'undefined' ? window : this);
