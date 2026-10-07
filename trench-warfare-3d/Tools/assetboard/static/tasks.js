// The tasks on the page: data/tasks.js (tasks.py) as groups of rows, what a click on one opens, and the things the
// owner can say about one. Pure functions, no DOM, so test_tasks.py runs them under node: a task he has queued or
// dropped shows so from his click, before the next reading says it too; a capture is told by when it was taken, the
// rest by how long nobody has been on them and when they stopped; and what is wrong with the reading itself (it
// failed, it is old, the Drive is away, a station was not heard from) is said in words for the top of the page.
(function (root) {
  // the kinds, in the order the page lists them (src_tasks.py KINDS), each with the words over its rows. `note` says
  // whose move a group is; `folded` groups open on a click (his own stops wait for nobody).
  var KINDS = [
    { key: 'capture', label: 'Your feedback from the game', note: 'These wait for your word, not for an agent: queue one for the relay, or say it is not needed.' },
    { key: 'agent', label: 'Agents that stopped before the end' },
    { key: 'session', label: 'Sessions that stopped mid-work' },
    { key: 'handoff', label: 'Handoffs nobody picked up' },
    { key: 'unit', label: 'Units written for the relay that the master has not queued' },
    { key: 'relay', label: 'Units the relay tried and did not finish' },
    { key: 'ready', label: 'Pipeline steps nobody took' },
    { key: 'paused', label: 'Sessions you stopped yourself', note: 'Your own doing, so they wait for no agent and are not counted above. Queue one if you want it finished.', folded: true }
  ];
  // what he can say about a task: a click leaves the second as a note of his (src_tasks.py QUEUE_SAY, DROP_SAY)
  var QUEUE = 'Queue this for the relay.', DROP = 'Not needed: drop this task.';
  var SAY = [{ label: 'Queue it for the relay', say: QUEUE }, { label: 'Not needed', say: DROP }];
  var BACK = [{ label: 'Take it back', say: DROP }];
  var OLD = 5, UNHEARD = 30;      // minutes: a reading older than this is said, and a station not heard from for this long
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  function span(min) { min = Math.max(0, Math.round(min || 0)); return min < 1 ? 'under a minute' : min < 120 ? min + ' min' : min < 2880 ? Math.round(min / 60) + ' h' : Math.round(min / 1440) + ' days'; }
  function n(k, one, many) { return k ? (k === 1 ? one : k + ' ' + many) : ''; }
  function clock(sec) { var d = new Date(sec * 1000); return ('0' + d.getHours()).slice(-2) + ':' + ('0' + d.getMinutes()).slice(-2); }
  // where a task stands once his own notes are counted: `said` are the texts of his open notes about it, which the
  // page knows from the click on, a reading or more before data/tasks.js does. A queued task he then drops is taken
  // back; one the relay already has stays the relay's.
  function state(r, said) {
    said = said || [];
    if (r.state === 'queued') return said.indexOf(DROP) >= 0 ? 'dropped' : 'queued';
    if (r.state !== 'left') return r.state;
    return said.indexOf(DROP) >= 0 ? 'dropped' : said.indexOf(QUEUE) >= 0 ? 'queued' : 'left';
  }
  // what the relay's last stop said of a unit, in words
  function verdict(v) { v = String(v || '').toLowerCase(); return v === 'fail' ? 'its check failed' : v === 'pass' ? 'its check passed' : v; }
  function chips(r, st) {
    var c = [];
    if (st === 'queued') c.push('queued for the relay');
    if (st === 'relay' && r.kind !== 'relay') c.push('with the relay' + (r.legs ? ', ' + n(r.legs, '1 leg run', 'legs run') : ''));
    if (r.fault) c.push('a fault');
    c.push(r.kind === 'capture' ? 'captured ' + span(r.idle) + ' ago' : 'nobody on it for ' + span(r.idle));
    if (r.stopped) c.push(r.kind === 'capture' ? r.stopped : r.kind === 'ready' ? 'ready since ' + r.stopped : r.kind === 'handoff' || r.kind === 'unit' ? 'written ' + r.stopped : 'stopped ' + r.stopped);
    if (r.where) c.push('on ' + r.where);
    if (r.lane) c.push(short(r.lane));
    if (r.kind === 'handoff' && r.file && !r.fault) c.push(String(r.file).replace(/^HANDOFF_AGENT_|\.md$/g, ''));          // two handoffs can share a topic: the file tells them apart
    if (r.kind === 'agent' && r.agent) c.push((r.agent === 'general-purpose' ? 'general' : r.agent) + ' agent');
    if (r.verdict) c.push(verdict(r.verdict));
    if (r.asks) c.push('asked you something');
    if (r.words && r.words.length) c.push(n(r.words.length, '1 note of yours', 'notes of yours'));
    return c;
  }
  // what he can say now: both while nothing was said, the way back while it is only queued, nothing once the relay has it
  function actions(r, st) { return st === 'left' ? SAY : st === 'queued' ? BACK : []; }
  // one row: what it is, a line under it, the chips, its picture when it has one, and the buttons it carries
  function row(r, said) {
    var st = state(r, said);
    return { id: r.id, top: r.title, sub: r.kind === 'capture' ? ((r.detail || [])[0] || '') : (r.what || ''), shot: r.shots && r.shots[0] ? r.shots[0].src : '', chips: chips(r, st), state: st,
             tip: (r.detail || [])[0] || r.title, acts: actions(r, st) };
  }
  // what a click on a row opens (board.js): the lines tasks.py wrote, the picture, and the same buttons the row has.
  // The subject is the task and nothing wider, so the notes under it are the notes about it.
  function subject(r, label, said) {
    var st = state(r, said);
    return { kind: 'queue', id: 'task: ' + r.id, kindLabel: 'task · ' + String(label || r.kind).toLowerCase(), title: r.title, facts: chips(r, st), detail: (r.detail || []).slice(0, 5),
             shots: (r.shots || []).slice(0, 3), wantsShots: r.kind === 'capture', actions: actions(r, st) };
  }
  // the groups that hold something, in order; a task he dropped is not listed
  function groups(T, saidOf) {
    var rows = (T && T.rows) || [];
    return KINDS.map(function (k) {
      var mine = rows.filter(function (r) { return r.kind === k.key && state(r, saidOf ? saidOf(r) : []) !== 'dropped'; });
      return { key: k.key, label: k.label, note: k.note || '', folded: !!k.folded, raw: mine, rows: mine.map(function (r) { return row(r, saidOf ? saidOf(r) : []); }) };
    }).filter(function (g) { return g.rows.length; });
  }
  function count(gs, st, pick) { return gs.reduce(function (k, g) { return k + (pick && !pick(g) ? 0 : g.rows.filter(function (r) { return r.state === st; }).length); }, 0); }
  function forAgent(g) { return g.key !== 'capture' && g.key !== 'paused'; }
  // the line under the page's title: what waits for him, what waits for an agent, what he queued, what the relay has
  function head(T, gs) {
    if (!T) return 'The tasks have not been read yet.';
    var mine = count(gs, 'left', function (g) { return g.key === 'capture'; }), left = count(gs, 'left', forAgent), queued = count(gs, 'queued'), relay = count(gs, 'relay', function (g) { return g.key !== 'relay'; }), out = [];
    if (mine) out.push(n(mine, '1 capture of yours waits for your word', 'captures of yours wait for your word'));
    out.push(left ? n(left, '1 task waits for an agent', 'tasks wait for an agent') : mine ? 'nothing waits for an agent' : 'Nothing is left unfinished');
    if (queued) out.push(n(queued, '1 queued for the relay', 'queued for the relay'));
    if (relay) out.push(n(relay, '1 with the relay', 'with the relay'));
    if (T.relay && T.relay.waiting) out.push('the relay\'s queue holds ' + n(T.relay.waiting, '1 unit', 'units') + (T.relay.as_of ? ' (its board last changed ' + T.relay.as_of + ')' : ''));
    return out.join(' · ');
  }
  // which stations the list was read on, and how long ago each
  function heard(T, nowSec) {
    return ((T && T.stations) || []).map(function (s) { var m = Math.max(0, (nowSec - s.at) / 60); return s.host + (m < 2 ? ' just now' : ' ' + span(m) + ' ago'); }).join(', ');
  }
  // what is wrong with the reading itself, each as a sentence for the top of the page: none when all is well
  function warnings(T, nowSec) {
    var out = [];
    if (!T) return out;
    if (T.failed) out.push('The tasks could not be read at ' + clock(T.failed.at) + ' (' + T.failed.why + '). What is below is from the reading before.');
    else if (T.read_at && (nowSec - T.read_at) / 60 > OLD) out.push('This list was read ' + span((nowSec - T.read_at) / 60) + ' ago and not since: the board\'s watcher may have stopped.');
    if (T.drive_away) out.push('The shared Drive is away on this station.' + (T.waiting_here ? ' ' + n(T.waiting_here, '1 capture waits', 'captures wait') + ' in the game\'s folder.' : '') + ' Nothing is taken in, and nothing you say here is taken up while it is away.');
    ((T.stations) || []).forEach(function (s) { var m = (nowSec - s.at) / 60; if (m > UNHEARD) out.push(s.host + ' was last read ' + span(m) + ' ago: its agents and sessions may be missing here.'); });
    return out;
  }
  // the first few rows for the control screen, a row of each group in turn, so no kind crowds the others out
  function firstOf(gs, most) {
    var out = [], depth = 0, more = true;
    while (more && out.length < most) {
      more = false;
      gs.forEach(function (g) { if (g.folded || out.length >= most) return; if (depth < g.rows.length) { out.push([g, depth]); more = true; } });
      depth++;
    }
    return out;
  }
  // the words under the rows: the rule, where the list was read, and what the board cannot see
  function foot(T, nowSec) {
    if (!T) return '';
    return 'A task is listed once nothing has touched it for ' + span(T.stale_minutes) + '; a capture from the game at once. Read on ' + heard(T, nowSec) + '.'
      + (T.dropped ? ' ' + T.dropped + ' you took off the list.' : '') + (T.blind ? ' ' + T.blind : '');
  }
  var api = { KINDS: KINDS, SAY: SAY, BACK: BACK, QUEUE: QUEUE, DROP: DROP, span: span, state: state, chips: chips, row: row, subject: subject, groups: groups, count: count, head: head, heard: heard, short: short,
              actions: actions, warnings: warnings, firstOf: firstOf, foot: foot, verdict: verdict };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.Tasks = api;
})(typeof window !== 'undefined' ? window : this);
