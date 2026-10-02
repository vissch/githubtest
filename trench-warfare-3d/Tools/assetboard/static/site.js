// The overview's filters: kind, level and a name search. Cards carry data-cat, data-levels and data-name;
// a section with no card left says so, and the counts follow.
(function () {
  var state = { cat: '', level: '', q: '' };
  function apply() {
    document.querySelectorAll('.bucket').forEach(function (bucket) {
      var shown = 0;
      bucket.querySelectorAll('.card').forEach(function (card) {
        var ok = (!state.cat || card.dataset.cat === state.cat) &&
                 (!state.level || (' ' + card.dataset.levels + ' ').indexOf(' ' + state.level + ' ') >= 0) &&
                 (!state.q || card.dataset.name.indexOf(state.q) >= 0);
        card.hidden = !ok;
        if (ok) shown++;
      });
      bucket.querySelector('.none').hidden = shown > 0;
      document.querySelectorAll('[data-count="' + bucket.id + '"]').forEach(function (n) { n.textContent = shown; });
    });
  }
  document.querySelectorAll('.chips[data-filter]').forEach(function (group) {
    group.querySelectorAll('.chip').forEach(function (chip) {
      chip.addEventListener('click', function () {
        group.querySelectorAll('.chip').forEach(function (c) { c.classList.remove('on'); });
        chip.classList.add('on');
        state[group.dataset.filter] = chip.dataset.v;
        apply();
      });
    });
  });
  var q = document.getElementById('q');
  if (q) q.addEventListener('input', function () { state.q = q.value.trim().toLowerCase(); apply(); });
})();
