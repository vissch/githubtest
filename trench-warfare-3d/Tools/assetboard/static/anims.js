// The Animations page: the H3 reference clips per unit (data/anims.js, made by anims.py). One row per unit, a column per action.
(function () {
  const A = window.ANIMS || { units: [], actions: [] };
  const $ = (id) => document.getElementById(id);
  const el = (t, cls, txt) => { const e = document.createElement(t); if (cls) e.className = cls; if (txt != null) e.textContent = txt; return e; };
  const state = { action: 'all', unit: 'all', auto: true, rej: false };
  const total = A.units.reduce((n, u) => n + u.clips.length, 0);
  $('an-count').textContent = total ? `${total} clips for ${A.units.length} units, from ${A.root}` : `No clips found in ${A.root || 'the Drive folder'}. Is the Drive mounted?`;

  const tabs = $('an-actions');
  ['all', ...A.actions].forEach((a) => {
    const b = el('button', 'an-tab', a === 'all' ? 'All actions' : a);
    b.type = 'button'; b.dataset.a = a;
    b.onclick = () => { state.action = a; draw(); };
    tabs.appendChild(b);
  });
  const sel = $('an-unit');
  sel.appendChild(new Option('All units', 'all'));
  A.units.forEach((u) => sel.appendChild(new Option(u.name, u.key)));
  sel.onchange = () => { state.unit = sel.value; draw(); };
  $('an-auto').onchange = (e) => { state.auto = e.target.checked; };
  $('an-rej').onchange = (e) => { state.rej = e.target.checked; draw(); };

  const big = $('an-big'), bigv = $('an-bigv');
  $('an-close').onclick = () => big.close();
  big.addEventListener('close', () => { bigv.pause(); bigv.removeAttribute('src'); bigv.load(); });
  function open(url, text) { bigv.src = url; $('an-bigp').textContent = text; big.showModal(); bigv.play().catch(() => {}); }

  function clip(url, label, prompt, pic) {
    const box = el('figure', 'an-clip');
    const v = el('video');
    v.muted = true; v.loop = true; v.playsInline = true; v.preload = 'none';
    if (pic) v.poster = pic; else v.src = url + '#t=2.5';
    v.onmouseenter = () => { if (!state.auto) return; if (!v.src) v.src = url; v.play().catch(() => {}); };
    v.onmouseleave = () => { v.pause(); };
    v.onclick = () => open(url, `${label}${prompt ? ' — ' + prompt : ''}`);
    v.onerror = () => box.classList.add('an-miss');
    box.appendChild(v);
    const cap = el('figcaption', null, label);
    if (prompt) cap.title = prompt;
    box.appendChild(cap);
    return box;
  }

  function draw() {
    tabs.querySelectorAll('button').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.a === state.action)));
    const board = $('an-board');
    board.textContent = '';
    const acts = state.action === 'all' ? A.actions : [state.action];
    A.units.filter((u) => state.unit === 'all' || u.key === state.unit).forEach((u) => {
      const sec = el('section', 'an-unitrow');
      const head = el('header', 'an-uhead');
      if (u.pic) { const i = el('img'); i.src = u.pic; i.alt = u.name; i.loading = 'lazy'; head.appendChild(i); }
      const t = el('div');
      t.appendChild(el('h3', null, u.name));
      t.appendChild(el('p', 'an-sub', `${u.clips.length} clips${u.rejected.length ? `, ${u.rejected.length} re-rolled` : ''}`));
      if (u.sheet) { const a = el('a', null, 'Review sheet'); a.href = u.sheet; a.target = '_blank'; t.appendChild(a); }
      head.appendChild(t);
      sec.appendChild(head);
      const grid = el('div', 'an-grid');
      grid.style.setProperty('--cols', acts.length);
      acts.forEach((a) => {
        const col = el('div', 'an-col');
        col.appendChild(el('h4', null, a));
        u.clips.filter((c) => c.action === a).forEach((c) => col.appendChild(clip(c.url, `${a} ${c.take}`, c.prompt, c.poster)));
        grid.appendChild(col);
      });
      sec.appendChild(grid);
      if (state.rej && u.rejected.length) {
        const r = el('div', 'an-rejrow');
        r.appendChild(el('h4', null, 'Re-rolled first takes'));
        const g = el('div', 'an-rejgrid');
        u.rejected.forEach((c) => g.appendChild(clip(c.url, c.name, '')));
        r.appendChild(g);
        sec.appendChild(r);
      }
      board.appendChild(sec);
    });
  }
  draw();
})();
