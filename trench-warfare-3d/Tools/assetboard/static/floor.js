// The branches page (floor.html): the open rooms and the lounge are drawn by office.js, like the overview's office;
// this draws the branches nobody is in as a strip, one bar per branch (newest first, its height the commits it is
// ahead, parked ones dimmed; hover for its last commit), with the full list folded underneath. office.js calls
// window.drawBranches with each reading of ops.py's data.
window.drawBranches = (function () {
  var last = '';
  return function (o) {
    var el = window.Crew.el;
    var quiet = o.lanes.filter(function (l) { return !(l.workers.length || l.items.length || l.dirty); });
    var sig = JSON.stringify(quiet.map(function (l) { return [l.branch, l.tip, l.ahead, l.live]; }));
    if (sig === last) return;
    last = sig;
    quiet.sort(function (a, b) { return (b.tip || '').localeCompare(a.tip || ''); });
    document.getElementById('n-quiet').textContent = quiet.length;
    var top = Math.max.apply(null, quiet.map(function (l) { return l.ahead || 0; }).concat([1]));
    var strip = document.getElementById('strip'); strip.innerHTML = '';
    quiet.forEach(function (l) {
      var a = el('span', 'k-bar1 ' + (l.live ? 'live' : 'parked'));
      a.style.setProperty('--v', Math.max(.08, Math.sqrt((l.ahead || 0) / top)));
      a.title = l.branch + ' · ' + (l.ahead || 0) + ' ahead · tip ' + (l.tip || '?') + (l.live ? '' : ' · parked') + (l.last[0] ? '\n' + l.last[0].subject : '');
      strip.appendChild(a);
    });
    // the three that are furthest ahead carry their names
    quiet.slice().sort(function (a, b) { return (b.ahead || 0) - (a.ahead || 0); }).slice(0, 3).forEach(function (l) {
      var i = quiet.indexOf(l), bar = strip.children[i];
      if (bar) bar.appendChild(el('span', 'k-bar-num', String(1 + quiet.slice().sort(function (a, b) { return (b.ahead || 0) - (a.ahead || 0); }).indexOf(l))));
      if (false) bar.appendChild(el('span', 'k-bar-name ' + (i < quiet.length / 2 ? 'k-right' : 'k-left') + ' k-lift' + quiet.slice().sort(function (a, b) { return (b.ahead || 0) - (a.ahead || 0); }).indexOf(l), l.branch.replace(/^lane\/(show|sim)\//, '') + ' · ' + l.ahead));
    });
    var top3 = document.getElementById('strip-top');
    if (top3) { top3.innerHTML = ''; top3.appendChild(el('span', 'k-soft', 'Furthest ahead')); quiet.slice().sort(function (a, b) { return (b.ahead || 0) - (a.ahead || 0); }).slice(0, 3).forEach(function (l, n) {
      var t = el('span', 'k-top3'); t.appendChild(el('b', null, String(n + 1))); t.appendChild(document.createTextNode(l.branch.replace(/^lane\/(show|sim)\//, '') + ' · ' + l.ahead + ' ahead')); top3.appendChild(t); }); }
    var q = document.getElementById('quiet'); q.innerHTML = '';
    quiet.forEach(function (l) {
      var r = el('div', 'qrow' + (l.live ? '' : ' parked')); r.appendChild(el('b', null, l.branch));
      r.appendChild(el('span', 'dim', (l.tip ? 'tip ' + l.tip : '') + (l.ahead ? ' · ' + l.ahead + ' ahead' : '') + (l.live ? '' : ' · parked')));
      r.appendChild(el('span', 'small', l.last[0] ? l.last[0].subject : ''));
      q.appendChild(r);
    });
  };
})();
