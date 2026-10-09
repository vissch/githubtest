// The relay's runs on the page: data/runs.js (src_runs.py, runreport.py) as what the page says of each. Pure
// functions, no DOM, so test_runs.py runs them under node: when a run was and how long, how it ended in plain words,
// what its units came to, which agent worked a unit (the relay's name for one is a role, a phase and a model), what
// the run leaves for the owner (the briefs that are its own, what a leg asked that no brief holds yet), whether its
// report has been written, and what is wrong with the reading itself.
(function (root) {
  var READ = 'Read this run and write its report.';        // what "Have it read" leaves as his note (runreport.py READ_SAY)
  var OLD = 5;                                             // minutes: a reading older than this is said
  var DAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'], MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  // why a run ended, by the relay's one word for it; the relay's own sentence is under it for the ones that need it
  var ENDS = { done: 'Nothing was left to do', hours: 'Its hours were up', legs: 'It reached its cap of legs', budget: 'The day\'s budget was spent', pace: 'The day\'s pace held the next unit back',
               checkout: 'The work checkout could not be used', leg: 'A leg did not run clean', uncommitted: 'A unit left uncommitted work behind', 'no-result': 'Units kept ending with no result',
               lane: 'Units\' lanes could not be switched to', error: 'It ended on an error' };
  var TOLD = { checkout: 1, leg: 1, uncommitted: 1, 'no-result': 1, lane: 1, error: 1, other: 1 };      // the kinds whose own sentence says what went wrong
  var HOW = { raised: 'this run left it for you', step: 'a step this run worked', lane: 'may come from this run: it is about a lane the run worked, and was asked after it', from: 'your answer queued work this run did' };
  // the relay's own sentence in plain words, where it is one he need not parse: a git error with this machine's paths says a lane was open elsewhere
  function plain(reason) {
    var s = String(reason || ''), m = /git switch -q (\S+) failed[\s\S]*already used by worktree at '([^']+)'/.exec(s);
    if (m) return 'The lane ' + short(m[1]) + ' was open in another checkout on that machine (' + m[2].split(/[\\/]/).pop() + '), so the relay could not switch to it.';
    return s;
  }
  // a time a report or a brief carries (2026-10-10 00:36, this station's) as the page says times
  function stamp(s) { var m = /^(\d{4})-(\d\d)-(\d\d)[ T](\d\d):(\d\d)/.exec(String(s || '')); return m ? day(new Date(+m[1], +m[2] - 1, +m[3]).getTime() / 1000) + ' ' + m[4] + ':' + m[5] : String(s || ''); }
  function two(k) { return ('0' + k).slice(-2); }
  function n(k, one, many) { return k === 1 ? one : k + ' ' + many; }
  function short(b) { return String(b || '').replace(/^lane\/(show|sim)\//, ''); }
  function money(usd) { return usd == null ? '' : '$' + (usd >= 100 ? Math.round(usd) : Number(usd).toFixed(2)); }
  function span(sec) { var m = Math.max(0, Math.round((sec || 0) / 60)); return m < 1 ? 'under a minute' : m < 60 ? m + ' min' : Math.floor(m / 60) + ' h' + (m % 60 ? ' ' + two(m % 60) : ''); }
  function clock(sec) { var d = new Date(sec * 1000); return two(d.getHours()) + ':' + two(d.getMinutes()); }
  function day(sec) { var d = new Date(sec * 1000); return DAYS[d.getDay()] + ' ' + d.getDate() + ' ' + MONTHS[d.getMonth()]; }
  // when a run was: the day, from when to when, and how long; a run that is going says since when
  function when(r, nowSec) {
    if (!r.started) return 'time unknown';
    var end = r.ended || 0, out = day(r.started) + ' · ' + clock(r.started);
    if (end) return out + ' to ' + (day(end) === day(r.started) ? '' : day(end) + ' ') + clock(end) + ' · ' + span(end - r.started);
    return r.empty ? out : out + ' · ' + (r.state === 'going' ? 'going for ' : 'began ') + span(Math.max(0, nowSec - r.started)) + (r.state === 'going' ? '' : ' ago');
  }
  // a model as he calls it: claude-opus-5 is Opus 5
  function model(m) {
    var p = String(m || '').replace(/^claude-/, '').split('-'); if (!p[0]) return '';
    return p[0].charAt(0).toUpperCase() + p[0].slice(1) + (p.length > 1 ? ' ' + p.slice(1).join('.') : '');
  }
  // a role as the page names the agent: the relay's role, or what a leg with no role brief is
  function role(name) { return name === 'lane' ? 'lane agent (no role brief)' : name === 'retro' ? 'retrospective' : name ? String(name).replace(/-/g, ' ') + ' agent' : 'agent'; }
  // which agent worked a unit: its role, then each phase with how many legs and on which model
  function agents(u) {
    var seen = [], last = null;
    (u.legs || []).forEach(function (l) { var k = l.phase + '|' + model(l.model) + '|' + (l.effort || ''); if (!last || last.k !== k) { last = { k: k, phase: l.phase, model: model(l.model), effort: l.effort || '', n: 0 }; seen.push(last); } last.n++; });
    return { role: role(u.role), phases: seen.map(function (x) { return (x.n > 1 ? x.n + ' ' + x.phase + ' legs' : x.phase) + (x.model ? ' on ' + x.model : '') + (x.effort ? ' ' + x.effort : ''); }) };
  }
  // who worked in a run: every role with its legs, the one with the most first
  function crew(r) {
    var by = {}, out = [];
    (r.units || []).forEach(function (u) { (u.legs || []).forEach(function (l) { var k = l.role || u.role || ''; if (!by[k]) { by[k] = { role: role(k), legs: 0 }; out.push(by[k]); } by[k].legs++; }); });
    return out.sort(function (a, b) { return b.legs - a.legs; });
  }
  // where a unit stands: the verdict of its check, or why it has none
  function verdict(u, r) {
    var v = String(u.verdict || '').toUpperCase();
    if (v === 'PASS') return { key: 'pass', label: 'passed' };
    if (v === 'FAIL') return { key: 'fail', label: 'failed' };
    if (v === 'BLOCKED') return { key: 'blocked', label: 'blocked' };
    if (u.source === 'retro' || u.role === 'retro') return { key: 'none', label: 'retrospective' };
    if (r.state !== 'ended') return r.now_on === u.id ? { key: 'going', label: 'being worked on' } : { key: 'none', label: 'no verdict yet' };
    return { key: 'cut', label: 'cut off' };
  }
  function unitLine(u, r) {
    var line = r.report && r.report.units && r.report.units[u.id], legs = u.legs || [], v = String(u.verdict || '').toUpperCase();
    if (line) return { top: line, read: true };
    if (!legs.length) return { top: v === 'PASS' ? 'Its work was already on its lane when the run came to it: the check passed and no leg was needed.' : v ? 'No leg ran on it: ' + ((u.why || [])[0] || 'the records do not say why') + '.' : 'No leg ran on it.', read: false };
    return { top: legs[legs.length - 1].result || u.id, read: false };
  }
  // why a unit failed or is blocked, when the records say; said when they do not, so a failure is never a bare word
  function whyNot(u) {
    var v = String(u.verdict || '').toUpperCase();
    if (v !== 'FAIL' && v !== 'BLOCKED') return [];
    return (u.why || []).length ? u.why : [v === 'FAIL' ? 'The records do not say why its check failed: the runner only printed that on the desktop.' : 'The records do not say what blocked it beyond its last leg\'s own words.'];
  }
  // what a run cost, or that nothing recorded it
  function cost(r) { return r.legs && r.unpriced >= r.legs ? 'cost not recorded' : money(r.usd) + (r.unpriced ? ' (' + n(r.unpriced, '1 leg', 'legs') + ' not priced)' : ''); }
  function tally(r) {
    var t = { pass: 0, fail: 0, blocked: 0, cut: 0, going: 0, none: 0 };
    (r.units || []).forEach(function (u) { t[verdict(u, r).key]++; });
    return t;
  }
  function tallyWords(r) {
    var t = tally(r), out = [];
    if (t.pass) out.push(t.pass + ' passed'); if (t.fail) out.push(t.fail + ' failed'); if (t.blocked) out.push(t.blocked + ' blocked');
    if (t.cut) out.push(t.cut + ' cut off'); if (t.going) out.push(t.going + ' being worked on');
    return out;
  }
  // why it ended, as { line, told }: the plain words, and the relay's own sentence when that says what went wrong
  function ended(r) {
    if (r.state === 'going') return { line: 'Going now' + (r.silent ? ', but nothing was heard from it for hours: it may have been cut off' : ''), told: '' };
    if (r.state !== 'ended') return { line: r.silent ? 'It left no stop record: it was cut off, or its end never reached the board' : 'It has no stop record yet: most likely still going', told: '' };
    if (r.kind === 'asked') return { line: r.asked_why ? 'Stopped on request: ' + r.asked_why : 'Stopped on request (the record does not say by whom)', told: '' };
    if (r.kind === 'hours') return { line: ENDS.hours + (/mid-leg/.test(r.reason || '') ? ', in the middle of a leg' : ''), told: '' };
    var own = TOLD[r.kind] || !ENDS[r.kind] ? String(r.reason || '') : '', said = plain(own);
    return said !== own ? { line: said, told: '' } : { line: ENDS[r.kind] || 'It ended', told: own };
  }
  // the run in a line, when nobody has written its report: what its units came to
  function title(r) {
    if (r.report && r.report.title) return r.report.title;
    if (r.empty) return 'No leg ran';
    var w = tallyWords(r), k = (r.units || []).length;
    return n(k, '1 unit', 'units') + (w.length ? ': ' + w.join(', ') : '');
  }
  // what a run leaves for him, sorted: the briefs that are its own and open (`open`), what a leg asked that no brief
  // holds yet (`asks`), open briefs guessed onto it by lane (`likely`), the ones he has answered (`decided`), the
  // answers of his that queued its work (`from`), and what the reader judged not his (`notHis`)
  function decisions(r) {
    var b = r.briefs || [], a = r.asks || [];
    return { open: b.filter(function (x) { return x.state !== 'answered' && (x.how === 'raised' || x.how === 'step'); }),
             likely: b.filter(function (x) { return x.state !== 'answered' && x.how === 'lane'; }),
             decided: b.filter(function (x) { return x.state === 'answered' && x.how !== 'from'; }),
             from: b.filter(function (x) { return x.how === 'from'; }),
             asks: a.filter(function (x) { return !x.brief && !x.no; }), notHis: a.filter(function (x) { return !!x.no; }) };
  }
  function waits(r) { var d = decisions(r); return d.open.length + d.asks.length; }
  // open decisions that are this run's only by a guess: drawn with their buttons, so they are counted and said, apart
  function maybe(r) { return decisions(r).likely.length; }
  function how(b) { return HOW[b.how] || ''; }
  // what he decided on a brief, in a line
  function decided(b) {
    var a = b.answer; if (!a) return '';
    var o = (b.options || []).filter(function (x) { return x.key === a.option; })[0];
    return 'You decided ' + String(a.when || '').slice(0, 10) + ': ' + (o ? a.option + ', ' + o.text : a.said ? '"' + a.said + '"' : a.option) + (a.queued ? ' · queued as ' + a.queued : a.outcome ? ' · ' + a.outcome : '');
  }
  // whether a run's report is written, being written, waiting, or can be asked for: { key, label, ask }
  function reading(r, R, said) {
    var g = (R && R.reading) || {};
    if (r.report) return { key: 'read', label: stamp(r.report.when), ask: false };
    if (r.empty || r.state !== 'ended') return { key: 'na', label: '', ask: false };
    if (g.running && g.running.run === r.id) return { key: 'now', label: 'An agent is reading this run now; its report shows here when it is done.', ask: false };
    if ((g.waiting || []).indexOf(r.id) >= 0 || (said || []).indexOf(READ) >= 0) return { key: 'next', label: 'Waits to be read' + (g.off ? ', and nothing is reading: ' + g.off : '') + '.', ask: false };
    if (r.gave_up) return { key: 'failed', label: 'An agent tried to write this run up and could not: ' + r.gave_up + '. Below is what its legs wrote themselves.', ask: true };
    return { key: 'bare', label: 'Nobody has written this run up. Below is what its legs wrote themselves.', ask: true };
  }
  // the rows of the page: a run, or the starts that failed one after another as one row
  function rows(R) {
    var out = [];
    ((R && R.runs) || []).forEach(function (r) {
      var last = out[out.length - 1];
      if (r.empty && last && last.starts) last.starts.push(r);
      else out.push(r.empty ? { starts: [r] } : { run: r });
    });
    return out;
  }
  // a row of starts that failed, in a line
  function starts(list) {
    var first = list[list.length - 1], last = list[0], why = plain(last.reason) || 'no reason recorded';
    return { top: n(list.length, 'A start that ran no leg', 'starts that ran no leg') + ', ' + day(first.started) + ' ' + clock(first.started) + (list.length > 1 ? ' to ' + clock(last.started) : ''), why: why };
  }
  // one leg, as the page lists it: who, how long, what for, what it said
  function leg(l) {
    var facts = [l.phase, role(l.role), model(l.model) + (l.effort ? ' ' + l.effort : ''), span(l.seconds)];
    if (l.usd != null) facts.push(money(l.usd));
    if (l.resumed) facts.push('given one more turn');
    if (l.state && l.state !== 'DONE') facts.push('the session ended ' + l.state);
    return { n: 'Leg ' + two(l.n), facts: facts.filter(Boolean), said: l.said ? l.said : 'no verdict line', key: l.said || 'none' };
  }
  // the line under the page's title
  function head(R) {
    if (!R) return 'The runs have not been read yet.';
    if (R.board === false) return 'This station has no pipeline board: the runs cannot be read here.';
    var rs = (R.runs || []).filter(function (r) { return !r.empty; }), going = rs.filter(function (r) { return r.state === 'going' || (r.state === 'open' && !r.silent); }).length;
    var more = rs.reduce(function (k, r) { return k + maybe(r); }, 0);
    var mine = rs.reduce(function (k, r) { return k + waits(r); }, 0), unread = rs.filter(function (r) { return r.state === 'ended' && !r.report; }).length, out = [];
    out.push(rs.length ? 'The last ' + n(rs.length, 'run', 'runs') : 'No run is on the board yet');
    if (going) out.push(n(going, '1 going now', 'going now'));
    out.push(mine ? n(mine, '1 thing waits on you from them', 'things wait on you from them') : more ? 'nothing is known to wait on you from them' : 'nothing from them waits on you');
    if (more) out.push(n(more, '1 open decision may come from them', 'open decisions may come from them'));
    if (unread) out.push(n(unread, '1 not written up', 'not written up'));
    return out.join(' · ');
  }
  // what is wrong with the reading itself, each as a sentence for the top of the page
  function warnings(R, nowSec) {
    var out = [];
    if (!R) return out;
    if (R.failed) out.push('The runs could not be read at ' + clock(R.failed.at) + ' (' + R.failed.why + '). What is below is from the reading before.');
    else if (R.read_at && (nowSec - R.read_at) / 60 > OLD) out.push('This list was read ' + span(nowSec - R.read_at) + ' ago and not since: the board\'s watcher may have stopped.');
    return out;
  }
  // the words under the rows: where this is read from and what it cannot see
  function foot(R) {
    if (!R || R.board === false) return '';
    return 'Read from the pipeline board as it stood ' + (R.as_of ? day(R.as_of) + ' ' + clock(R.as_of) : 'at a time unknown') + (R.commit ? ' (' + R.commit + ')' : '')
      + '. The relay writes a run\'s records there; a run that is going shows what it has pushed so far. Times are this station\'s. A run that ended is written up by an agent, the newest first; an older one when you ask.';
  }
  // what the page says where the runs would be, when there are none to draw
  function none(R, only) {
    if (!R) return 'The runs have not been read yet.';
    if (R.board === false) return 'This station has no pipeline board: the runs cannot be read here.';
    if (R.failed && !(R.runs || []).length) return 'The runs could not be read, and no earlier reading is kept.';
    return only ? 'No run of the last ' + (R.most || 20) + ' has an open decision for you.' : 'No relay run is on the pipeline board yet.';
  }
  // the strip over the page: a bar a run, as tall as its cost, coloured by what waits on him or how it went
  function strip(R) {
    var rs = ((R && R.runs) || []).filter(function (r) { return !r.empty; }), top = rs.reduce(function (m, r) { return Math.max(m, r.usd || 0); }, 0) || 1;
    return rs.map(function (r) {
      var t = tally(r), key = r.state !== 'ended' ? 'going' : waits(r) || maybe(r) ? 'you' : t.fail || t.blocked ? 'bad' : 'ok';
      return { id: r.id, key: key, height: Math.max(8, Math.round(100 * (r.usd || 0) / top)), tip: day(r.started) + ' ' + clock(r.started) + ' · ' + title(r) + ' · ' + money(r.usd) + (waits(r) ? ' · ' + n(waits(r), '1 waits on you', 'wait on you') : '') };
    }).reverse();
  }
  function subject(r) { return { kind: 'queue', id: 'run: ' + r.id, kindLabel: 'relay run', title: title(r) }; }
  var api = { READ: READ, n: n, short: short, money: money, span: span, clock: clock, day: day, when: when, model: model, role: role, agents: agents, crew: crew, verdict: verdict, tally: tally, tallyWords: tallyWords,
              ended: ended, title: title, decisions: decisions, waits: waits, maybe: maybe, unitLine: unitLine, whyNot: whyNot, cost: cost, plain: plain, stamp: stamp, none: none, how: how, decided: decided, reading: reading, rows: rows, starts: starts, leg: leg, head: head, warnings: warnings, foot: foot,
              strip: strip, subject: subject };
  if (typeof module !== 'undefined' && module.exports) module.exports = api; else root.Runs = api;
})(typeof window !== 'undefined' ? window : this);
