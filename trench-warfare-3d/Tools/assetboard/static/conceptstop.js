// The concepts on the overview (index.html #concepts-top): the pictures he approved last, and a line of counts and of what
// he asked the desktop to make. It reads data/concepts-state.js only (concepts.py writes a few approved pictures into it),
// and borrows the words of the page's own script (concepts.js: Concepts.plural, Concepts.requestLine). The state is read
// again every 20 seconds.
(function (root) {
  var C = root.Concepts || (typeof require !== 'undefined' ? require('./concepts.js') : null);
  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  // the line under the pictures: how many there are and how many are approved, then what is being made or waits
  function line(S) {
    var top = (S || {}).top || {}, reqs = ((S || {}).requests || []).filter(function (r) { return r.state === 'queued' || r.state === 'rendering' || r.state === 'failed'; });
    var out = [C.plural(top.total || 0, 'picture', 'pictures') + ', ' + (top.approved || 0) + ' approved'];
    reqs.slice(-2).forEach(function (r) { out.push(r.title + ' · ' + C.requestLine(r)); });
    return out.join('  ·  ');
  }
  if (typeof module !== 'undefined' && module.exports) { module.exports = { line: line }; return; }
  var sig = '';
  function draw() {
    var S = root.CONCEPT_STATE, sec = document.getElementById('concepts-top');
    if (!S || !sec || !S.top || !C) return;
    var key = JSON.stringify([S.top, S.requests]);
    if (key === sig) return; sig = key;
    sec.hidden = false;
    document.getElementById('cct-n').textContent = '· ' + S.top.approved + ' approved';
    document.getElementById('cct-line').textContent = line(S);
    var home = document.getElementById('cct-rows'); home.textContent = '';
    if (!S.top.recent.length) { home.appendChild(el('p', 'cct-none', 'Nothing approved yet. Open the page and press Approve on a picture.')); return; }
    S.top.recent.forEach(function (it) {
      var a = el('a', 'cct-pic'); a.href = 'concepts.html'; a.title = it.n;
      var img = el('img'); img.loading = 'lazy'; img.alt = it.n; img.src = 'img/concept/' + it.th; img.onerror = function () { img.removeAttribute('src'); };
      a.appendChild(img); a.appendChild(el('span', null, it.n)); home.appendChild(a);
    });
  }
  draw();
  setInterval(function () {
    var s = document.createElement('script'); s.src = 'data/concepts-state.js?t=' + Date.now();
    s.onload = s.onerror = function () { s.remove(); draw(); }; document.body.appendChild(s);
  }, 20000);
})(typeof window !== 'undefined' ? window : this);
