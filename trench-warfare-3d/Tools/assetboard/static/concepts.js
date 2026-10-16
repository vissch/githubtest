// The concept page (concepts.html): the pictures ComfyUI made for the project (data/concepts.js, concepts.py), a chip a
// session to filter by, an Approve button on every picture with the approved ones in a section of their own, and a box
// that asks the desktop for new ones. data/concepts-state.js (approved, the requests, the stamp of the pictures) is read
// again every few seconds; the pictures themselves only when their stamp changes. An approval or a request goes to the
// notes listener (notes.py, /concept/...) with the key beside the page. The words are pure functions, so test_concepts.py
// runs them under node; the drawing is below them.
(function (root) {
  var STEP = 240;                               // cards drawn at a time
  var STATES = { queued: 'Waiting', rendering: 'Rendering', done: 'Done', failed: 'Failed', cancelled: 'Cancelled' };
  function plural(k, one, many) { return k + ' ' + (k === 1 ? one : many); }
  // the rows of data/concepts.js as pictures: {id, s, c, cl, t, n, i, k, seed, p (the prompt), th (the thumbnail's file)}
  function pictures(C) {
    var f = (C || {}).fields || [], pr = (C || {}).prompts || [];
    return ((C || {}).rows || []).map(function (r) {
      var o = {}; f.forEach(function (name, j) { o[name] = r[j]; });
      o.p = pr[o.p] || ''; return o;
    });
  }
  // the prompt a description becomes: this must say what concepts.py build_prompt says (test_concepts.py holds them equal)
  function compose(B, kind, text, theme) {
    var body = String(text || '').replace(/\s+/g, ' ').trim().replace(/[.,;]+$/, '');
    body = body.replace(/^@clashroyale,?\s*(3d render,?\s*)?/i, '');
    return B.trigger + body + (B.themes[theme || ''] || '') + B.kinds[kind];
  }
  // what a filter lets through: f = {sessions: [ids] (none: all), cat, q, hide}, ap = {id: when}
  function visible(list, f, ap) {
    var sess = f.sessions || [], words = String(f.q || '').toLowerCase().split(/\s+/).filter(Boolean);
    return list.filter(function (it) {
      if (sess.length && sess.indexOf(it.s) < 0) return false;
      if (f.cat && it.c !== f.cat) return false;
      if (f.hide && ap && ap[it.id]) return false;
      if (!words.length) return true;
      var hay = (it.n + ' ' + it.t + ' ' + it.p + ' ' + it.cl).toLowerCase();
      return words.every(function (w) { return hay.indexOf(w) >= 0; });
    });
  }
  // the categories of the pictures of the picked sessions, as [id, words], and how many pictures each holds
  function categories(list, sess) {
    var seen = {}, out = [];
    list.forEach(function (it) {
      if (sess && sess.length && sess.indexOf(it.s) < 0) return;
      if (!seen[it.c]) { seen[it.c] = [it.c, it.cl, 0]; out.push(seen[it.c]); }
      seen[it.c][2]++;
    });
    return out;
  }
  function sessionCount(list, id, cat) { return list.filter(function (it) { return it.s === id && (!cat || it.c === cat); }).length; }
  // the original on the Drive, for a page opened as a file (a page from elsewhere may not read it: the thumbnail stands in)
  function fullUrl(rootPath, id) { return 'file:///' + rootPath + '/' + String(id).split('/').map(encodeURIComponent).join('/'); }
  function shortLabel(s) { return String(s || '').replace(/^\d+ - /, ''); }
  // one line for the counts over the page
  function count(list, ap) {
    if (!list.length) return 'No pictures yet.';
    var n = 0; list.forEach(function (it) { if (ap[it.id]) n++; });
    return plural(list.length, 'picture', 'pictures') + ', ' + n + ' approved';
  }
  // a request in his words: what state it is in and what it waits for
  function requestLine(r) {
    var s = STATES[r.state] || r.state;
    if (r.state === 'rendering') return s + ': ' + (r.got || 0) + ' of ' + r.takes + ' pictures';
    if (r.state === 'done') return s + ': ' + plural(r.got || r.takes, 'picture', 'pictures') + ' in Prompted';
    if (r.state === 'queued') return r.why ? r.why.charAt(0).toUpperCase() + r.why.slice(1) : s;
    return s + (r.why ? ': ' + r.why : '');
  }
  function pending(ap, pend) {              // what is approved as the page shows it: the Drive's answer, with his clicks that it does not know yet over it
    var o = {}; Object.keys(ap || {}).forEach(function (k) { o[k] = ap[k]; });
    Object.keys(pend || {}).forEach(function (k) { if (pend[k].on) o[k] = o[k] || 'now'; else delete o[k]; });
    return o;
  }
  // which of his clicks the state has caught up with (or that are older than a minute and a half): those stop standing over it
  function settle(ap, pend, now) {
    var left = {}; Object.keys(pend || {}).forEach(function (k) { if (!!(ap || {})[k] !== pend[k].on && now - pend[k].at < 90000) left[k] = pend[k]; });
    return left;
  }
  // a request as the page last saw it from the box (at) against the state's copy: his own answer stands for a minute and a half
  function mergeRequests(mine, theirs, now) {
    var by = {}; (theirs || []).forEach(function (r) { by[r.id] = r; });
    (mine || []).forEach(function (m) { if (now - m.at < 90000 || !by[m.r.id]) by[m.r.id] = m.r; });
    return Object.keys(by).map(function (k) { return by[k]; });
  }
  var api = { STEP: STEP, STATES: STATES, plural: plural, pictures: pictures, compose: compose, visible: visible, categories: categories, sessionCount: sessionCount,
              fullUrl: fullUrl, shortLabel: shortLabel, count: count, requestLine: requestLine, pending: pending, settle: settle, mergeRequests: mergeRequests };
  if (typeof module !== 'undefined' && module.exports) { module.exports = api; return; }
  root.Concepts = api;

  // ---- the drawing (only on the page that has the box: the overview borrows the words above, conceptstop.js)
  if (typeof document === 'undefined' || !document.getElementById('cc-new')) return;
  var $ = function (id) { return document.getElementById(id); };
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  var LS = 'tw3d-concepts';
  var pend = {}, sig = {}, limit = STEP, list = [], kind = 'vehicle', edited = false, openId = '', local = [];
  var view = { sessions: [], cat: '', q: '', hide: false };
  try { var kept = JSON.parse(localStorage.getItem(LS) || '{}'); view.sessions = kept.sessions || []; view.cat = kept.cat || ''; } catch (e) { /* private window */ }
  function keep() { try { localStorage.setItem(LS, JSON.stringify({ sessions: view.sessions, cat: view.cat })); } catch (e) { /* kept until the page closes */ } }
  function changed(key, s) { if (sig[key] === s) return false; sig[key] = s; return true; }
  function S() { return root.CONCEPT_STATE || { approved: {}, requests: [], builder: null }; }
  function ap() { return pending(S().approved, pend); }
  function rootPath() { return (root.CONCEPTS || {}).root || ''; }
  function sessionById(id) { return ((root.CONCEPTS || {}).sessions || []).filter(function (s) { return s.id === id; })[0] || { id: id, label: id, about: '' }; }
  function say(text, bad) { var s = $('cc-say'); s.textContent = text || ''; s.classList.toggle('cc-bad', !!bad); }
  function warn(text) { var w = $('cc-warn'); w.hidden = !text; w.textContent = text || ''; }

  // what is sent to the listener (the box the notes use): plain text, so the browser asks no question first
  function post(path, body) {
    var b = root.NOTEBOX && root.NOTEBOX.url ? root.NOTEBOX : null;
    if (!b) return Promise.reject(new Error('the page does not know where the note box is'));
    body.key = b.key;
    return fetch(b.url + path, { method: 'POST', headers: { 'Content-Type': 'text/plain' }, body: JSON.stringify(body) })
      .then(function (r) { return r.json(); }).then(function (d) { if (!d.ok) throw new Error(d.why || 'refused'); return d; },
        function (e) { throw new Error(e.message === 'Failed to fetch' ? 'the note box does not answer (it runs while ops.py --watch runs; the page that reads this was built before it had the concept routes?)' : e.message); });
  }
  function setApproved(ids, on) {
    ids.forEach(function (id) { pend[id] = { on: on, at: Date.now() }; });
    drawAll();
    return post('/concept/approve', { ids: ids, approved: on }).then(function (d) {
      S().approved = d.approved; pend = settle(d.approved, pend, Date.now()); warn(''); drawAll();
    }, function (e) { ids.forEach(function (id) { delete pend[id]; }); warn('Not saved: ' + e.message); drawAll(); });
  }

  // ---- a picture
  function card(it, approved) {
    var ok = !!approved[it.id], c = el('div', 'cc-card' + (ok ? ' cc-ok' : ''));
    var img = el('img', 'cc-pic'); img.loading = 'lazy'; img.alt = it.n; img.src = 'img/concept/' + it.th; img.dataset.open = it.id;
    img.onerror = function () { img.onerror = null; img.removeAttribute('src'); img.classList.add('cc-wait'); };     // a thumbnail the watcher has not made yet
    c.appendChild(img);
    var m = el('div', 'cc-meta'); m.appendChild(el('b', 'cc-name', it.n)); m.appendChild(el('span', 'cc-type', it.t)); c.appendChild(m);
    var foot = el('div', 'cc-foot');
    foot.appendChild(el('span', 'k-qchip', shortLabel(sessionById(it.s).label))); foot.appendChild(el('span', 'k-qchip', it.cl));
    var b = el('button', 'cc-ap' + (ok ? ' on' : ''), ok ? 'Approved ✓' : 'Approve'); b.type = 'button'; b.dataset.ap = it.id; b.setAttribute('aria-pressed', ok ? 'true' : 'false');
    foot.appendChild(b); c.appendChild(foot);
    return c;
  }
  function fill(home, items, approved, none) {
    home.textContent = '';
    if (!items.length) { home.appendChild(el('p', 'cc-none', none)); return; }
    items.forEach(function (it) { home.appendChild(card(it, approved)); });
  }

  // ---- the filters
  function drawFilters() {
    var C = root.CONCEPTS || {}, sess = C.sessions || [];
    var s = JSON.stringify([sess, view.sessions, view.cat, list.length]);
    if (changed('tabs', s)) {
      var bar = $('cc-sessions'); bar.textContent = '';
      sess.forEach(function (x) {
        var on = view.sessions.indexOf(x.id) >= 0, t = el('button', 'cc-tab' + (on ? ' on' : '') + (x.count ? '' : ' none'), x.label);
        t.type = 'button'; t.setAttribute('aria-pressed', on ? 'true' : 'false'); t.appendChild(el('span', 'cc-tab-n', String(sessionCount(list, x.id, view.cat))));
        t.onclick = function () { var i = view.sessions.indexOf(x.id); if (i < 0) view.sessions.push(x.id); else view.sessions.splice(i, 1); limit = STEP; keep(); drawAll(); };
        bar.appendChild(t);
      });
      if (view.sessions.length) { var all = el('button', 'cc-tab cc-tab-all', 'All sessions'); all.type = 'button'; all.onclick = function () { view.sessions = []; limit = STEP; keep(); drawAll(); }; bar.appendChild(all); }
      var about = sess.filter(function (x) { return view.sessions.indexOf(x.id) >= 0; }).map(function (x) { return x.about; }).join('  ·  ');
      $('cc-about').textContent = view.sessions.length ? about : 'All sessions are shown.';
    }
    var cats = categories(list, view.sessions);
    if (view.cat && !cats.some(function (c) { return c[0] === view.cat; })) { view.cat = ''; keep(); }
    if (changed('cats', JSON.stringify([cats, view.cat]))) {
      var sel = $('cc-cat'); sel.textContent = '';
      var o0 = el('option', null, 'All categories'); o0.value = ''; sel.appendChild(o0);
      cats.forEach(function (c) { var o = el('option', null, c[1] + ' (' + c[2] + ')'); o.value = c[0]; if (c[0] === view.cat) o.selected = true; sel.appendChild(o); });
    }
  }
  function drawGrids() {
    var approved = ap(), everything = visible(list, view, approved), mine = visible(list, { sessions: view.sessions, cat: view.cat, q: view.q }, approved);
    var yes = mine.filter(function (it) { return approved[it.id]; }).sort(function (a, b) { return String(approved[b.id]).localeCompare(String(approved[a.id])); });
    var rest = everything;
    $('cc-ap-n').textContent = String(yes.length); $('cc-all-n').textContent = String(rest.length);
    var filtered = view.sessions.length || view.cat || view.q;
    var s1 = JSON.stringify(yes.map(function (it) { return it.id; }));
    if (changed('ap', s1)) fill($('cc-ap'), yes, approved, 'Nothing approved' + (filtered ? ' in this filter' : ' yet') + '. Press Approve on a picture below.');
    var shown = rest.slice(0, limit), s2 = JSON.stringify([shown.map(function (it) { return it.id + (approved[it.id] ? '+' : ''); }), rest.length]);
    if (changed('all', s2)) fill($('cc-all'), shown, approved, list.length ? 'No pictures match this filter.' : 'No pictures yet.');
    var more = $('cc-more'); more.hidden = rest.length <= limit; more.textContent = 'Show ' + Math.min(STEP, rest.length - limit) + ' more (' + (rest.length - limit) + ' left)';
    $('cc-count').textContent = count(list, approved);
    return everything;
  }

  // ---- the box that makes new ones
  function drawKinds() {
    var B = S().builder; if (!B) return;
    if (!changed('kinds', JSON.stringify(B.labels) + kind)) return;
    var bar = $('cc-kinds'); bar.textContent = '';
    (B.order || Object.keys(B.labels)).forEach(function (k) {
      var b = el('button', 'cc-tab' + (k === kind ? ' on' : ''), B.labels[k]); b.type = 'button'; b.setAttribute('aria-pressed', k === kind ? 'true' : 'false');
      b.onclick = function () { kind = k; sig.kinds = null; drawKinds(); remake(); };
      bar.appendChild(b);
    });
    var sel = $('cc-takes');
    if (!sel.options.length) for (var i = 1; i <= (B.most || 8); i++) { var o = el('option', null, String(i)); o.value = String(i); if (i === 4) o.selected = true; sel.appendChild(o); }
  }
  function remake() {                       // the made prompt follows his words until he writes in it himself
    var B = S().builder, t = $('cc-text').value.trim();
    var go = $('cc-go'); go.disabled = t.length < 8;
    if (!B || edited) return;
    $('cc-prompt').value = t ? compose(B, kind, t, $('cc-frog').checked ? 'frog' : '') : '';
  }
  function drawRequests() {
    var all = mergeRequests(local, S().requests, Date.now()).sort(function (a, b) { return String(b.when).localeCompare(String(a.when)); }).slice(0, 8);
    if (!changed('reqs', JSON.stringify(all))) return;
    var home = $('cc-reqs'); home.textContent = '';
    all.forEach(function (r) {
      var c = el('div', 'cc-req cc-s-' + r.state), h = el('div', 'cc-req-h');
      h.appendChild(el('b', null, r.title)); h.appendChild(el('span', 'k-qchip', (S().builder && S().builder.labels[r.kind]) || r.kind));
      h.appendChild(el('span', 'k-qchip cc-state', STATES[r.state] || r.state)); c.appendChild(h);
      c.appendChild(el('p', 'cc-req-line', requestLine(r)));
      var acts = el('div', 'cc-req-acts');
      function act(label, fn) { var b = el('button', 'cc-btn', label); b.type = 'button'; b.onclick = function () { b.disabled = true; fn().then(function () { b.disabled = false; }); }; acts.appendChild(b); }
      if (r.state === 'queued') act('Cancel', function () { return change(r, 'cancel'); });
      if (r.state === 'failed' || r.state === 'cancelled') act('Retry', function () { return change(r, 'retry'); });
      if (r.state === 'done') act('Show them', function () { view.sessions = ['prompted']; view.cat = ''; view.q = ''; $('cc-find').value = ''; limit = STEP; keep(); drawAll(); $('cc-all-h').scrollIntoView(); return Promise.resolve(); });
      if (acts.children.length) c.appendChild(acts);
      home.appendChild(c);
    });
  }
  function remember(r) { local = local.filter(function (x) { return x.r.id !== r.id; }); local.push({ r: r, at: Date.now() }); sig.reqs = null; }
  function change(r, what) {
    return post('/concept/' + what, { id: r.id }).then(function (d) { remember(d.request); drawRequests(); },
      function (e) { say('Not done: ' + e.message, true); });
  }
  function make(ev) {
    ev.preventDefault();
    var t = $('cc-text').value.trim(); if (t.length < 8) return;
    var body = { text: t, kind: kind, takes: +$('cc-takes').value, theme: $('cc-frog').checked ? 'frog' : '' };
    if (edited) body.prompt = $('cc-prompt').value;
    $('cc-go').disabled = true; say('Sending…');
    post('/concept/request', body).then(function (d) {
      remember(d.request); $('cc-text').value = ''; edited = false; remake(); drawRequests();
      say('Queued. The desktop starts it when its ComfyUI answers; the pictures appear under "Prompted" as they land.');
    }, function (e) { say('Not sent: ' + e.message, true); remake(); });
  }

  // ---- one picture large
  function lightbox(id) {
    var it = list.filter(function (x) { return x.id === id; })[0], box = $('cc-lb');
    if (!it) { closeLb(); return; }
    openId = id; box.hidden = false; box.textContent = '';
    var img = el('img'); img.alt = it.n; img.src = fullUrl(rootPath(), it.id);
    img.onerror = function () { img.onerror = null; img.src = 'img/concept/' + it.th; };       // the original is not reachable from here: the thumbnail
    var side = el('div', 'cc-side'), ok = !!ap()[id];
    side.appendChild(el('h3', null, it.n)); side.appendChild(el('p', 'cc-type', it.t));
    side.appendChild(el('p', 'cc-facts', [sessionById(it.s).label, it.cl, it.i ? 'idea ' + it.i : '', 'take ' + it.k, it.seed ? 'seed ' + it.seed : ''].filter(Boolean).join(' · ')));
    if (it.p) side.appendChild(el('p', 'cc-prompt', it.p));
    var row = el('div', 'cc-acts');
    var a = el('button', 'cc-ap' + (ok ? ' on' : ''), ok ? 'Approved ✓ (click to undo)' : 'Approve'); a.type = 'button'; a.dataset.ap = id; row.appendChild(a);
    if (it.p) { var m = el('button', 'cc-btn', 'Use this prompt'); m.type = 'button'; m.title = 'Put its prompt in the box above, to make more like it';
      m.onclick = function () { edited = true; $('cc-prompt').value = it.p; $('cc-fulld').open = true; closeLb(); $('cc-new').scrollIntoView(); say('Its prompt is in the box. Pick what it is, then press Make it.'); }; row.appendChild(m); }
    var x = el('button', 'cc-btn', 'Close'); x.type = 'button'; x.onclick = closeLb; row.appendChild(x);
    side.appendChild(row); box.appendChild(img); box.appendChild(side);
  }
  function closeLb() { openId = ''; var box = $('cc-lb'); box.hidden = true; box.textContent = ''; }
  var order = [];
  function step(d) { var i = order.indexOf(openId); if (i < 0 || !order.length) return; lightbox(order[(i + d + order.length) % order.length]); }

  function drawAll() {
    if (!root.CONCEPTS) { $('cc-count').textContent = 'The pictures are not in the site yet (the watcher writes them: ops.py --watch).'; return; }
    drawKinds(); drawFilters(); order = drawGrids().map(function (it) { return it.id; }); drawRequests();
    var miss = S().thumbs_missing; $('cc-count').textContent += miss ? ' · ' + plural(miss, 'thumbnail is', 'thumbnails are') + ' still being made' : '';
    if (openId) lightbox(openId);
    if (root.Crew && root.Crew.nowPill && root.OPS) root.Crew.nowPill(root.OPS);
  }

  // ---- wiring
  document.addEventListener('click', function (e) {
    var t = e.target; if (!t || !t.dataset) return;
    if (t.dataset.ap) return setApproved([t.dataset.ap], !ap()[t.dataset.ap]);
    if (t.dataset.open) return lightbox(t.dataset.open);
    if (t.id === 'cc-lb') return closeLb();
  });
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') closeLb();
    if (openId && (e.key === 'ArrowRight' || e.key === 'ArrowLeft')) step(e.key === 'ArrowRight' ? 1 : -1);
  });
  $('cc-cat').onchange = function (e) { view.cat = e.target.value; limit = STEP; keep(); drawAll(); };
  $('cc-find').oninput = function (e) { view.q = e.target.value; limit = STEP; drawAll(); };
  $('cc-hide').onchange = function (e) { view.hide = e.target.checked; drawAll(); };
  $('cc-more').onclick = function () { limit += STEP; drawAll(); };
  $('cc-text').oninput = remake; $('cc-frog').onchange = remake;
  $('cc-prompt').oninput = function () { edited = true; };
  $('cc-reset').onclick = function () { edited = false; remake(); };
  $('cc-new').onsubmit = make;

  function load() { list = pictures(root.CONCEPTS); sig = {}; drawAll(); }
  function script(file, then) {
    var s = document.createElement('script'); s.src = file + '?t=' + Date.now();
    s.onload = s.onerror = function () { s.remove(); if (then) then(); }; document.body.appendChild(s);
  }
  var stamp = (root.CONCEPT_STATE || {}).items;
  load(); remake();
  // the state again every few seconds (a request's progress, an approval from another tab); the pictures when their stamp moved
  setInterval(function () {
    script('data/concepts-state.js', function () {
      pend = settle(S().approved, pend, Date.now());
      var now = (root.CONCEPT_STATE || {}).items;
      if (now && now !== stamp) { stamp = now; script('data/concepts.js', load); } else drawAll();
    });
  }, 6000);
  if (root.Crew && root.Crew.live) root.Crew.live(function () { if (root.Crew.nowPill && root.OPS) root.Crew.nowPill(root.OPS); });
})(typeof window !== 'undefined' ? window : this);
