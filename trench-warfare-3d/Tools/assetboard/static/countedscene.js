// The office, counted, drawn: a floor in three.js that can be turned (graphs.html, and the control screen).
// counted.js says what stands where and what every label says; this file only draws it. It needs three.min.js
// (r160, in the site beside it: a page opened from the Drive as a file cannot ask a CDN, and need not) and WebGL.
// mount() gives nothing back when the browser has neither, and counted.js then shows the list alone.
// A drag turns the floor (with a finger: sideways; up and down stays the page's scroll), two fingers or Ctrl and the
// wheel bring it nearer, a click or a tap on a stack of crates picks its branch. The arrow keys, + and - do the same
// on the picture when it has the focus, and the buttons under it (counted.js sends them here as act()).
// The frogs are the two sheets of data/frogtex.js; without them a worker is a small green figure.
// The crates fall into place once, the first time the floor is on the screen, and the view drifts a little while they
// do; after that nothing moves but the frogs until someone takes hold. With less motion asked for, nothing moves.
(function () {
  'use strict';
  var CR = 0.8, GAP = 0.06, END = 7;          // a crate's edge, the gap between two, the seconds until everything has landed
  // the blues of a week, the oldest day the darkest; today's crates are orange
  var WEEK = [0x6272b8, 0x6a7bc2, 0x7384c8, 0x7f8fd0, 0x8c9bdb, 0x9aa8e4, 0xaab6ee];

  function mount(stage, opt) {
    var THREE = window.THREE, canvas = stage.querySelector('canvas'), box = stage.querySelector('.o-labels');
    opt = opt || {};
    if (!THREE || !canvas || !box) return null;
    var renderer;
    try { renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true }); } catch (e) { return null; }
    var gl = renderer.getContext();
    try { var info = gl.getExtension('WEBGL_debug_renderer_info'); stage.setAttribute('data-gl', String(info ? gl.getParameter(info.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER))); } catch (e) { /* the name of the card is for whoever checks the page, not for the page */ }
    var css = getComputedStyle(stage), BG = new THREE.Color(css.getPropertyValue('--o-floor').trim() || '#111624'), FIRE = css.getPropertyValue('--o-fire').trim() || '#ff4a1c';
    renderer.setPixelRatio(Math.min(2, window.devicePixelRatio || 1));
    renderer.shadowMap.enabled = true; renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    renderer.toneMapping = THREE.ACESFilmicToneMapping; renderer.toneMappingExposure = 1.15;
    var scene = new THREE.Scene(); scene.background = BG; scene.fog = new THREE.Fog(BG, 38, 95);
    var camera = new THREE.PerspectiveCamera(30, 16 / 9, 0.5, 300);

    // ---- light
    scene.add(new THREE.HemisphereLight(0xa9bcff, 0x0b0e16, 1.5));
    var sun = new THREE.DirectionalLight(0xffffff, 2.6); sun.position.set(-14, 30, 18); sun.castShadow = true;
    sun.shadow.mapSize.set(2048, 2048); var sc = sun.shadow.camera; sc.left = -28; sc.right = 28; sc.top = 28; sc.bottom = -28; sc.near = 1; sc.far = 90; sun.shadow.bias = -0.0006; sun.shadow.radius = 5;
    scene.add(sun);
    var warm = new THREE.PointLight(0xff5a2a, 260, 40, 2); warm.position.set(0, 6, 12); scene.add(warm);
    var cool = new THREE.PointLight(0x5b9dff, 1400, 70, 2); cool.position.set(10, 14, -16); scene.add(cool);

    // ---- textures drawn here
    function tex(w, h, draw) { var c = document.createElement('canvas'); c.width = w; c.height = h; draw(c.getContext('2d'), w, h); var t = new THREE.CanvasTexture(c); t.colorSpace = THREE.SRGBColorSpace; t.anisotropy = 8; return t; }
    var grid = tex(256, 256, function (g, w) { g.fillStyle = '#151b2b'; g.fillRect(0, 0, w, w); g.strokeStyle = 'rgba(160,178,235,.16)'; g.lineWidth = 2; g.strokeRect(0, 0, w, w); });
    grid.wrapS = grid.wrapT = THREE.RepeatWrapping; grid.repeat.set(60, 60);
    var crateFace = tex(256, 256, function (g, w) {
      g.fillStyle = '#e9ecf8'; g.fillRect(0, 0, w, w);
      g.strokeStyle = '#ffffff'; g.lineWidth = 26; g.strokeRect(13, 13, w - 26, w - 26);
      g.strokeStyle = 'rgba(40,50,90,.35)'; g.lineWidth = 5; g.strokeRect(27, 27, w - 54, w - 54);
      g.lineWidth = 16; g.strokeStyle = '#ffffff'; g.beginPath(); g.moveTo(30, 30); g.lineTo(w - 30, w - 30); g.stroke();
      g.lineWidth = 4; g.strokeStyle = 'rgba(40,50,90,.3)'; g.beginPath(); g.moveTo(40, 26); g.lineTo(w - 26, w - 40); g.moveTo(26, 40); g.lineTo(w - 40, w - 26); g.stroke();
    });
    var glow = tex(128, 128, function (g, w) { var r = g.createRadialGradient(64, 64, 0, 64, 64, 64); r.addColorStop(0, 'rgba(255,255,255,1)'); r.addColorStop(0.4, 'rgba(255,255,255,.35)'); r.addColorStop(1, 'rgba(255,255,255,0)'); g.fillStyle = r; g.fillRect(0, 0, w, w); });
    var rugTex = tex(1024, 320, function (g, w, h) {
      g.fillStyle = FIRE; g.fillRect(0, 0, w, h); g.fillStyle = '#ffffff'; g.translate(w / 2, h / 2); g.beginPath();
      for (var i = 0; i < 16; i++) { var a = i * Math.PI / 8, r = i % 2 ? 34 : 96; g.lineTo(Math.cos(a) * r, Math.sin(a) * r); } g.closePath(); g.globalAlpha = 0.92; g.fill();
    });
    // the frogs: a sheet each, written out in data/frogtex.js with how its frames lie on it
    var sheets = {};
    Object.keys(opt.frogs || {}).forEach(function (k) {
      var f = opt.frogs[k]; if (!f || !f.src) return;
      var sheet = { n: f.n, cols: f.cols, rows: Math.ceil(f.n / f.cols), fps: f.fps || 8, ratio: f.w / f.h, loaded: false };
      // a frog made before its sheet has loaded shares the sheet's picture: it is told when the picture is there
      sheet.tex = new THREE.TextureLoader().load(f.src, function () { sheet.loaded = true; frogs.forEach(function (g) { if (g.sheet === sheet) g.tex.needsUpdate = true; }); touch(); });
      sheet.tex.colorSpace = THREE.SRGBColorSpace; sheet.tex.magFilter = THREE.LinearFilter;
      sheets[k] = sheet;
    });

    // ---- what never changes: the floor, and the shapes and paints the things are made of
    var floor = new THREE.Mesh(new THREE.PlaneGeometry(240, 240), new THREE.MeshStandardMaterial({ map: grid, roughness: 0.62, metalness: 0.1 }));
    floor.rotation.x = -Math.PI / 2; floor.receiveShadow = true; scene.add(floor);
    var G = {
      crate: new THREE.BoxGeometry(CR, CR, CR), pallet: new THREE.BoxGeometry(2, 0.14, 2), sheet: new THREE.BoxGeometry(1.1, 0.034, 1.5), plane: new THREE.PlaneGeometry(1, 1),
      shell: new THREE.CylinderGeometry(0.16, 0.16, 0.8, 20), tip: new THREE.ConeGeometry(0.16, 0.42, 20), bag: new THREE.SphereGeometry(0.5, 18, 12),
      tray: new THREE.EdgesGeometry(new THREE.BoxGeometry(1.5, 0.5, 1.9)), trayFloor: new THREE.BoxGeometry(1.5, 0.04, 1.9),
      body: new THREE.CylinderGeometry(0.22, 0.3, 0.7, 14), head: new THREE.SphereGeometry(0.3, 16, 12)
    };
    var M = {
      blue: new THREE.MeshStandardMaterial({ map: crateFace, roughness: 0.45, emissive: 0x3a4a9a, emissiveIntensity: 0.35 }),
      hot: new THREE.MeshStandardMaterial({ map: crateFace, color: 0xff5a2a, roughness: 0.45, emissive: 0xff3a0c, emissiveIntensity: 0.75 }),
      pallet: new THREE.MeshStandardMaterial({ color: 0x232b40, roughness: 0.7 }),
      sheet: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.8, emissive: 0x8e98b4, emissiveIntensity: 0.25 }),
      rug: new THREE.MeshStandardMaterial({ map: rugTex, roughness: 0.9 }),
      tray: new THREE.LineBasicMaterial({ color: 0xffffff }), trayFloor: new THREE.MeshStandardMaterial({ color: 0x141925, roughness: 0.9 }),
      brass: new THREE.MeshStandardMaterial({ color: 0xe8b550, metalness: 0.55, roughness: 0.3, emissive: 0x6a4410, emissiveIntensity: 0.6 }),
      tip: new THREE.MeshStandardMaterial({ color: 0xd06a34, metalness: 0.5, roughness: 0.35, emissive: 0x5a2208, emissiveIntensity: 0.6 }),
      bag: new THREE.MeshStandardMaterial({ color: 0x66709a, roughness: 1 }),
      frog: new THREE.MeshStandardMaterial({ color: 0x6fae4a, roughness: 0.6 })
    };

    // ---- what a reading makes: everything in `world`, thrown away and made again when a number changed
    var world = null, own = [], drops = [], labels = [], hits = [], frogs = [], fitPoints = [], got = {}, cur = null;
    var M4 = new THREE.Matrix4(), P3 = new THREE.Vector3(), S3 = new THREE.Vector3(1, 1, 1), Q4 = new THREE.Quaternion(), E3 = new THREE.Euler(), V = new THREE.Vector3(), COL = new THREE.Color();
    function bounce(x) { var n = 7.5625, d = 2.75; if (x < 1 / d) return n * x * x; if (x < 2 / d) { x -= 1.5 / d; return n * x * x + 0.75; } if (x < 2.5 / d) { x -= 2.25 / d; return n * x * x + 0.9375; } x -= 2.625 / d; return n * x * x + 0.984375; }
    function rnd(i) { var x = Math.sin(i * 127.1 + 311.7) * 43758.5453; return x - Math.floor(x); }
    function add(o) { world.add(o); return o; }
    function inst(geo, mat, n) { var m = new THREE.InstancedMesh(geo, mat, Math.max(1, n)); m.count = n; m.castShadow = m.receiveShadow = true; m.frustumCulled = false; own.push(m); return add(m); }
    function put(mesh, id, x, y, z, ry, sx, sy, sz, rx, rz) { E3.set(rx || 0, ry || 0, rz || 0); Q4.setFromEuler(E3); P3.set(x, y, z); S3.set(sx || 1, sy || 1, sz || 1); M4.compose(P3, Q4, S3); mesh.setMatrixAt(id, M4); mesh.instanceMatrix.needsUpdate = true; }
    function disc(x, z, r, colour, a) { var mat = new THREE.MeshBasicMaterial({ map: glow, color: colour, transparent: true, opacity: a, blending: THREE.AdditiveBlending, depthWrite: false }); own.push(mat); var m = new THREE.Mesh(G.plane, mat); m.scale.set(r * 2, r * 2, 1); m.rotation.x = -Math.PI / 2; m.position.set(x, 0.03, z); return add(m); }
    function label(cls, at, side, dy) {
      var e = document.createElement('div'); e.className = 'o-lab ' + cls;
      var b = document.createElement('b'), s = document.createElement('span'), sm = document.createElement('small');
      e.appendChild(b); e.appendChild(s); e.appendChild(sm); box.appendChild(e);
      var l = { e: e, b: b, s: s, sm: sm, at: at, side: side, dy: dy || 0, w: 0, h: 0, n: null, lift: 0, lead: null };
      if (side === 'above') { l.lead = document.createElement('u'); e.appendChild(l.lead); }
      labels.push(l); return l;
    }
    function say(l, n) { if (l.n !== n) { l.n = n; l.b.textContent = n; l.w = 0; } }
    function clear() {
      if (world) scene.remove(world);
      own.forEach(function (o) { if (o.dispose) o.dispose(); });
      labels.forEach(function (l) { l.e.remove(); });
      world = new THREE.Group(); scene.add(world);
      own = []; drops = []; labels = []; hits = []; frogs = []; fitPoints = [];
    }
    function sprite(sheet, w, x, z, i) {
      var t = sheet.tex.clone(); if (sheet.loaded) t.needsUpdate = true; t.repeat.set(1 / sheet.cols, 1 / sheet.rows); own.push(t);
      var mat = new THREE.SpriteMaterial({ map: t, transparent: true, alphaTest: 0.05 }); own.push(mat);
      var sp = new THREE.Sprite(mat); sp.center.set(0.5, 0); sp.scale.set(w, w / sheet.ratio, 1); sp.position.set(x, 0, z);
      frogs.push({ sheet: sheet, tex: t, phase: Math.floor(rnd(i * 7.7) * sheet.n) });
      return add(sp);
    }
    function figure(x, z, lying) {          // a worker without the owner's art: a small green figure, standing or lying
      var g = new THREE.Group(), b = new THREE.Mesh(G.body, M.frog), h = new THREE.Mesh(G.head, M.frog);
      b.position.y = 0.35; h.position.y = 0.92; b.castShadow = h.castShadow = true; g.add(b); g.add(h);
      if (lying) { g.rotation.z = Math.PI / 2; g.position.set(x + 0.5, 0.3, z); } else g.position.set(x, 0, z);
      return add(g);
    }

    function build(T, P, L) {
      clear();
      cur = { T: T, P: P };
      var narrow = P.narrow, N = T.stations.length, nBlue = 0, nHot = 0;
      sun.shadow.mapSize.set(narrow ? 1024 : 2048, narrow ? 1024 : 2048);
      T.stations.forEach(function (s) { s.count = s.full + (s.part ? 1 : 0); nHot += s.hot; nBlue += s.count - s.hot; });
      var blue = inst(G.crate, M.blue, nBlue), hot = inst(G.crate, M.hot, nHot), ib = 0, ih = 0, tallest = 0;
      // ---- a pallet of crates for each branch, and its frog
      T.stations.forEach(function (s, i) {
        var at = P.stations[i], x = at.x, z = at.z, pal = add(new THREE.Mesh(G.pallet, M.pallet)), words = L.stations[i];
        pal.position.set(x, 0.07, z); pal.castShadow = pal.receiveShadow = true;
        if (s.hot) disc(x, z, 3.2, 0xff4a1c, 0.35);
        s.landed = 0;
        for (var k = 0; k < s.count; k++) (function (k) {
          var isHot = k >= s.count - s.hot, mesh = isHot ? hot : blue, id = isHot ? ih++ : ib++, h = (k === s.count - 1 && s.part) ? s.part : 1;
          var layer = Math.floor(k / 4), q = k % 4, px = x + (q % 2 ? 1 : -1) * (CR + GAP) / 2, pz = z + (q < 2 ? -1 : 1) * (CR + GAP) / 2, py = 0.14 + layer * (CR + 0.02) + CR * h / 2;
          var rot = (rnd(i * 97 + k) - 0.5) * 0.16;
          if (!isHot) mesh.setColorAt(id, COL.setHex(WEEK[Math.min(WEEK.length - 1, s.day[k] + (WEEK.length - s.days.length))] || WEEK[3]));
          drops.push({ at: 0.4 + i * 0.14 + k * 0.045, len: 0.55, set: function (u) {
            put(mesh, id, px, u <= 0 ? -50 : py + (1 - bounce(Math.min(1, u))) * 9, pz, rot, 1, h, 1);
            if (u >= 1) s.landed = Math.max(s.landed, k + 1);
          } });
        })(k);
        s.top = 0.14 + Math.ceil(s.count / 4) * (CR + 0.02); tallest = Math.max(tallest, s.top);
        s.side = at.side || 'above';
        s.lab = s.side === 'above' ? label('o-stk' + (words.hot ? ' o-hot' : '') + (words.work ? ' o-work' : ''), new THREE.Vector3(x, s.top + 0.25, z), 'above', -4)
          : label('o-stk o-floor' + (words.hot ? ' o-hot' : '') + (words.work ? ' o-work' : ''), new THREE.Vector3(x, 0, z + 2.5), 'below', 2);
        s.lab.s.textContent = words.name; s.lab.sm.textContent = words.small; s.lab.e.title = s.branch;
        if (words.work) { var dot = document.createElement('i'); s.lab.s.insertBefore(dot, s.lab.s.firstChild); }
        s.lab.e.addEventListener('click', function () { if (opt.pick) opt.pick(s.branch); });
        // its frog: at the desk when someone is at work on the branch now, asleep by the pallet when nobody is
        var working = s.crew.length > 0, sheet = sheets[working ? 'office' : 'sleep_loop'];
        if (sheet) sprite(sheet, working ? 2.5 : 1.6, x + (working ? -0.1 : 0.1), z + (working ? 1.75 : 1.6), i);
        else figure(x, z + 1.7, !working);
        hits.push({ branch: s.branch, box: new THREE.Box3(new THREE.Vector3(x - 1.15, 0, z - 1.15), new THREE.Vector3(x + 1.15, Math.max(s.top, 0.6), z + 1.15)) });
        hits.push({ branch: s.branch, box: new THREE.Box3(new THREE.Vector3(x - 1.2, 0, z + 1.15), new THREE.Vector3(x + 1.2, working ? 2.2 : 1.1, z + 2.3)) });
        fitPoints.push([x, s.top + (s.side === 'above' ? (narrow ? 2.6 : 2.2) : 0.3), z], [x - 1.6, 0, z + 2.4], [x + 1.6, 0, z + 2.4]);
      });
      if (blue.instanceColor) blue.instanceColor.needsUpdate = true;

      // ---- the owner's table: what waits on him (in the tray), what is answered and closed, what waits on the crew
      var rug = add(new THREE.Mesh(G.plane, M.rug)); rug.scale.set(P.rug.w, P.rug.d, 1); rug.rotation.x = -Math.PI / 2; rug.position.set(P.rug.x, 0.02, P.rug.z); rug.receiveShadow = true;
      disc(P.rug.x, P.rug.z, P.rug.w * 0.56, 0xff4a1c, 0.16);
      var tray = add(new THREE.LineSegments(G.tray, M.tray)); tray.position.set(P.tray.x, 0.27, P.tray.z);
      var tf = add(new THREE.Mesh(G.trayFloor, M.trayFloor)); tf.position.set(P.tray.x, 0.04, P.tray.z); tf.receiveShadow = true;
      var piles = [['waiting', P.tray, 0xffc2a8], ['closed', P.closed, 0xffffff], ['crew', P.crew, 0xffffff]], per = (window.Counted || {}).STACK || 80, total = 0;
      piles.forEach(function (p) { total += Math.min(T.sheets[p[0]], per * 3); });
      var paper = inst(G.sheet, M.sheet, total), is = 0;
      got = { waiting: 0, closed: 0, crew: 0, shells: 0, bags: 0 };
      piles.forEach(function (p, pi) {
        var n = Math.min(T.sheets[p[0]], per * 3), stacks = Math.max(1, Math.ceil(n / per));
        for (var k = 0; k < n; k++) (function (k) {
          var id = is++, st = Math.floor(k / per), px = p[1].x + (st - (stacks - 1) / 2) * 1.25, pz = p[1].z, py = 0.06 + (k % per) * 0.036, rot = (rnd(k * 5.3 + pi) - 0.5) * 0.5, ox = (rnd(k * 3.1 + pi) - 0.5) * 0.12, oz = (rnd(k * 9.7 + pi) - 0.5) * 0.12;
          paper.setColorAt(id, COL.setHex(p[2]));
          drops.push({ at: 2.6 + pi * 0.25 + k * 0.025, len: 0.5, set: function (u) {
            var e = u <= 0 ? 0 : 1 - Math.pow(1 - Math.min(1, u), 3);
            put(paper, id, px + ox, u <= 0 ? -50 : py + (1 - e) * 6, pz + oz, rot + (1 - e) * 3, 1, 1, 1, (1 - e) * 1.2, (1 - e) * 0.8);
            if (u >= 1) got[p[0]]++;
          } });
        })(k);
      });
      if (paper.instanceColor) paper.instanceColor.needsUpdate = true;
      var tagZ = P.rug.z + P.rug.d / 2 + 0.35, tg = {};
      tg.waiting = label('o-tag o-hot', new THREE.Vector3(P.tray.x, 0, tagZ), 'below', 4);
      tg.closed = label('o-tag', new THREE.Vector3(P.closed.x, 0, tagZ), 'below', 4);
      tg.crew = label('o-tag', new THREE.Vector3(P.crew.x, 0, tagZ), 'below', 4);
      fitPoints.push([P.rug.x - P.rug.w / 2, 0, tagZ + (narrow ? 2.4 : 1.6)], [P.rug.x + P.rug.w / 2, 0, tagZ + (narrow ? 2.4 : 1.6)]);

      // ---- the relay's legs: a rank of shells
      var nS = T.shells ? T.shells.n : 0, shells = inst(G.shell, M.brass, nS), tips = inst(G.tip, M.tip, nS), SX = P.shells.x, SZ = P.shells.z, sc2 = P.shells.cols, sRows = Math.max(1, Math.ceil(nS / sc2));
      for (var k = 0; k < nS; k++) (function (k) {
        var px = SX + (k % sc2) * 0.46, pz = SZ + Math.floor(k / sc2) * 0.5;
        drops.push({ at: 3.7 + k * 0.035, len: 0.4, set: function (u) {
          var y = u <= 0 ? -50 : (1 - bounce(Math.min(1, u))) * 4;
          put(shells, k, px, y + 0.4, pz); put(tips, k, px, y + 1.01, pz); if (u >= 1) got.shells++;
        } });
      })(k);
      if (nS) disc(SX + (sc2 - 1) * 0.23, SZ + sRows * 0.25, 4.2, 0xffb03a, 0.22);
      tg.shells = label('o-tag' + (T.shells ? '' : ' o-none'), new THREE.Vector3(SX + (sc2 - 1) * 0.23, 0, SZ + (sRows - 1) * 0.5 + 0.9), 'below', 4);
      fitPoints.push([SX + (sc2 - 1) * 0.46 + 0.8, 0, SZ + sRows * 0.5 + (narrow ? 3.2 : 2.2)], [SX - 0.6, 1.6, SZ]);

      // ---- what landed: a wall of sandbags
      var nB = T.bags.n, bags = inst(G.bag, M.bag, nB), BX = P.bags.x, BZ = P.bags.z, bc = P.bags.cols;
      for (k = 0; k < nB; k++) (function (k) {
        var row = Math.floor(k / bc), col = k % bc, px = BX + col * 1.12 + (row % 2 ? 0.56 : 0), py = 0.25 + row * 0.44, rot = (rnd(k * 2.7) - 0.5) * 0.2;
        drops.push({ at: 4.0 + k * 0.04, len: 0.45, set: function (u) {
          put(bags, k, px, u <= 0 ? -50 : py + (1 - bounce(Math.min(1, u))) * 5, BZ, rot, 1.15, 0.5, 0.8); if (u >= 1) got.bags++;
        } });
      })(k);
      tg.bags = label('o-tag', new THREE.Vector3(BX + (bc - 1) * 0.56 + 0.28, 0, BZ + 1.1), 'below', 4);
      fitPoints.push([BX - 0.8, 0, BZ + 2.6], [BX - 0.8, Math.ceil(nB / bc) * 0.44 + 0.6, BZ], [BX + bc * 1.12 + 0.4, 0, BZ + 2.6]);
      Object.keys(tg).forEach(function (key) { tg[key].s.remove(); tg[key].sm.textContent = L.tags[key].small; tg[key].key = key; });
      cur.tags = tg; cur.L = L; cur.tallest = tallest;
      fit();
    }

    // ---- the camera: turns round the middle of the floor, as near as shows all of it in the frame it has
    var view = { az: 0, el: 0.3, zoom: 1, held: false, r: 44 }, target = new THREE.Vector3(), t = 0;
    function place(az, el, r) {
      camera.position.set(target.x + Math.sin(az) * Math.cos(el) * r, target.y + Math.sin(el) * r, target.z + Math.cos(az) * Math.cos(el) * r);
      camera.lookAt(target); camera.updateMatrixWorld(); camera.matrixWorldInverse.copy(camera.matrixWorld).invert();
    }
    function fits(az, el, r) {
      place(az, el, r);
      for (var i = 0; i < fitPoints.length; i++) { var p = fitPoints[i]; V.set(p[0], p[1], p[2]).project(camera); if (Math.abs(V.x) > 0.96 || V.y > 0.92 || V.y < -0.92) return false; }
      return true;
    }
    function near(P) { var r = 14; while (r < 160 && !(fits(-P.sweep, P.el, r) && fits(P.sweep, P.el, r) && fits(0, P.el, r))) r *= 1.03; return r; }
    function fit() {
      if (!cur) return;
      var P = cur.P, w = stage.clientWidth || 1, h = stage.clientHeight || 1, r, i, k;
      camera.aspect = w / h; camera.fov = P.narrow ? 34 : 30; camera.updateProjectionMatrix();
      target.set(P.narrow ? 0 : -0.6, P.narrow ? Math.max(1, Math.min(3, cur.tallest * 0.25)) : Math.max(1.6, Math.min(4.4, cur.tallest * 0.42)), (P.half.back + P.half.front) / 2);
      // as near as shows everything, then the eye moved along the floor until the things stand in the middle of the
      // frame's height, and nearer again: a few rounds settle it
      for (k = 0; k < 5; k++) {
        r = near(P); place(0, P.el, r);
        var lo = 1, hi = -1;
        for (i = 0; i < fitPoints.length; i++) { var p = fitPoints[i]; V.set(p[0], p[1], p[2]).project(camera); lo = Math.min(lo, V.y); hi = Math.max(hi, V.y); }
        target.z -= (hi + lo) / 2 * r * Math.tan(camera.fov * Math.PI / 360) / Math.sin(P.el);
      }
      view.r = near(P);
      if (!view.held) { view.el = P.el; view.zoom = 1; }
      scene.fog.near = view.r * 0.9; scene.fog.far = view.r * 2.3;
    }
    function ease(u) { u = Math.max(0, Math.min(1, u)); return u * u * (3 - 2 * u); }
    function drift() { return cur ? -cur.P.sweep + 2 * cur.P.sweep * ease(t / END) : 0; }

    // ---- a frame
    var dirty = true, clock = 0, frame = -1;
    function draw() {
      if (!cur) return;
      var w = stage.clientWidth, h = stage.clientHeight, T = cur.T, tg = cur.tags, L = cur.L, i;
      if (!w || !h) return;
      got.waiting = got.closed = got.crew = got.shells = got.bags = 0; T.stations.forEach(function (s) { s.landed = 0; });
      for (i = 0; i < drops.length; i++) { var d = drops[i]; d.set((t - d.at) / d.len); }
      var done = t >= END;
      T.stations.forEach(function (s, k) {
        say(s.lab, done || s.landed >= s.count ? L.stations[k].n : String(s.landed * T.crate).replace(/\B(?=(\d{3})+(?!\d))/g, ','));
        if (s.side === 'above') s.lab.at.y = Math.max(s.crew.length ? 2.6 : 1.3, 0.14 + Math.ceil(Math.max(1, done ? s.count : s.landed) / 4) * (CR + 0.02) + 0.25);
      });
      ['waiting', 'closed', 'crew'].forEach(function (key) { say(tg[key], done || got[key] >= Math.min(T.sheets[key], 240) ? L.tags[key].n : String(got[key])); });
      say(tg.shells, !T.shells || done || got.shells >= T.shells.n ? L.tags.shells.n : '$' + got.shells * T.shell);
      say(tg.bags, done || got.bags >= T.bags.n ? L.tags.bags.n : String(got.bags * T.bag));
      frogs.forEach(function (f) { var n = (Math.floor(clock * f.sheet.fps) + f.phase) % f.sheet.n; f.tex.offset.set((n % f.sheet.cols) / f.sheet.cols, 1 - (Math.floor(n / f.sheet.cols) + 1) / f.sheet.rows); });
      if (!view.held) view.az = drift();
      place(view.az, view.el, view.r * view.zoom);
      renderer.render(scene, camera);
      // the words: each at its thing. Where two would lie on each other the one nearer the eye stays where it is, and
      // the other is lifted clear of it on a thin line down to its stack; one that finds no room is left out (the list
      // has it). A label under its thing is not moved: it is kept or left out.
      var placed = [], order = labels.map(function (l) { V.copy(l.at).project(camera); l.x = (V.x * 0.5 + 0.5) * w; l.y = (-V.y * 0.5 + 0.5) * h + l.dy; l.z = V.z; l.d = camera.position.distanceTo(l.at); return l; }).sort(function (a, b) { return a.d - b.d; });
      order.forEach(function (l) {
        if (!l.w) { l.w = l.e.offsetWidth; l.h = l.e.offsetHeight; }
        var x0 = l.x - l.w / 2, x1 = x0 + l.w, y0 = l.side === 'above' ? l.y - l.h : l.y, lift = 0, hide = l.z >= 1 || x1 < 0 || x0 > w, tries = 0;
        var gap = l.side === 'above' ? 9 : 2;          // a label that can be lifted keeps its distance; one on the floor only must not touch
        function hit(top) { for (var i = 0; i < placed.length; i++) { var p = placed[i]; if (x0 < p[2] + gap && x1 > p[0] - gap && top < p[3] + 2 && top + l.h > p[1] - 2) return p; } return null; }
        for (var p = hide ? null : hit(y0); p; p = hit(y0 - lift)) {
          if (l.side !== 'above' || ++tries > 4) { hide = true; break; }
          lift = y0 + l.h - p[1] + 5;
        }
        if (y0 - lift < 2 && lift) hide = true;
        l.e.style.transform = 'translate(' + x0.toFixed(1) + 'px,' + (y0 - lift).toFixed(1) + 'px)';
        if (l.lead && l.lift !== lift) { l.lift = lift; l.lead.style.height = Math.round(lift) + 'px'; }
        if (l.hide !== hide) { l.hide = hide; l.e.classList.toggle('o-hid', hide); }
        if (!hide) placed.push([x0, y0 - lift, x1, y0 - lift + l.h]);
      });
    }

    // ---- the clock: runs while the floor is on the screen, draws when something moved
    var raf = 0, on = !('IntersectionObserver' in window), lastT = 0, alive = true;
    function tick(now) {
      raf = 0;
      if (!alive) return;
      var dt = Math.min(0.05, Math.max(0, (now - lastT) / 1000)); lastT = now;
      if (!opt.calm) {
        if (t < END + 0.1) { t += dt; dirty = true; }
        clock += dt;
        var f = Math.floor(clock * 12); if (frogs.length && f !== frame) { frame = f; dirty = true; }
      }
      if (dirty) { dirty = false; draw(); }
      if (on && !document.hidden) raf = requestAnimationFrame(tick);
    }
    function wake() { if (!raf && on && !document.hidden && alive) { lastT = performance.now(); raf = requestAnimationFrame(tick); } }
    function touch() { dirty = true; wake(); }
    if ('IntersectionObserver' in window) new IntersectionObserver(function (es) { on = es[es.length - 1].isIntersecting; wake(); }, { threshold: 0.05 }).observe(stage);
    document.addEventListener('visibilitychange', wake);
    function size() {
      var w = stage.clientWidth, h = stage.clientHeight; if (!w || !h) return;
      renderer.setSize(w, h, false); fit(); labels.forEach(function (l) { l.w = 0; }); touch();
    }
    if ('ResizeObserver' in window) new ResizeObserver(size).observe(stage); else window.addEventListener('resize', size);

    // ---- hands: a drag turns, two fingers or Ctrl and the wheel come nearer, a click picks a stack
    var ray = new THREE.Raycaster(), ndc = new THREE.Vector2(), hitAt = new THREE.Vector3(), fingers = {}, drag = null, pinch = 0;
    function hold() { if (!view.held) { view.az = drift(); view.held = true; } }
    function turn(daz, del) { hold(); view.az += daz; view.el = Math.max(0.08, Math.min(1.35, view.el + del)); touch(); }
    function zoom(k) { hold(); view.zoom = Math.max(0.35, Math.min(1.8, view.zoom * k)); touch(); }
    function under(e) {
      var b = canvas.getBoundingClientRect(), best = null, bd = Infinity;
      ndc.set((e.clientX - b.left) / b.width * 2 - 1, -((e.clientY - b.top) / b.height) * 2 + 1); ray.setFromCamera(ndc, camera);
      hits.forEach(function (hbox) { if (ray.ray.intersectBox(hbox.box, hitAt)) { var d = hitAt.distanceTo(camera.position); if (d < bd) { bd = d; best = hbox.branch; } } });
      return best;
    }
    function count() { return Object.keys(fingers).length; }
    function spread() { var k = Object.keys(fingers); return k.length < 2 ? 0 : Math.hypot(fingers[k[0]].x - fingers[k[1]].x, fingers[k[0]].y - fingers[k[1]].y); }
    canvas.addEventListener('pointerdown', function (e) {
      fingers[e.pointerId] = { x: e.clientX, y: e.clientY };
      try { canvas.setPointerCapture(e.pointerId); } catch (err) { /* a pointer that is already gone */ }
      if (count() === 1) drag = { x: e.clientX, y: e.clientY, az: view.held ? view.az : drift(), el: view.el, moved: 0, id: e.pointerId, t: Date.now() };
      else { drag = null; pinch = spread(); }
    });
    canvas.addEventListener('pointermove', function (e) {
      if (fingers[e.pointerId]) fingers[e.pointerId] = { x: e.clientX, y: e.clientY };
      if (count() >= 2 && pinch) { var s = spread(); if (s > 0) { zoom(pinch / s); pinch = s; } return; }
      if (!drag || drag.id !== e.pointerId) { if (!count()) canvas.style.cursor = under(e) ? 'pointer' : ''; return; }
      var dx = e.clientX - drag.x, dy = e.clientY - drag.y; drag.moved = Math.max(drag.moved, Math.abs(dx) + Math.abs(dy));
      if (drag.moved < 5) return;
      hold(); view.az = drag.az - dx * 0.006; view.el = Math.max(0.08, Math.min(1.35, drag.el + dy * 0.005)); touch();
    });
    function up(e) {
      var was = drag && drag.id === e.pointerId ? drag : null;
      delete fingers[e.pointerId]; if (count() < 2) pinch = 0; if (was) drag = null;
      if (was && e.type === 'pointerup' && was.moved < 5 && Date.now() - was.t < 600) { var b = under(e); if (b && opt.pick) opt.pick(b); }
    }
    canvas.addEventListener('pointerup', up); canvas.addEventListener('pointercancel', up);
    canvas.addEventListener('wheel', function (e) { if (!e.ctrlKey && !e.metaKey) return; e.preventDefault(); zoom(e.deltaY > 0 ? 1.08 : 0.93); }, { passive: false });
    function act(what) {
      if (what === 'left') turn(-0.2, 0); else if (what === 'right') turn(0.2, 0); else if (what === 'up') turn(0, 0.12); else if (what === 'down') turn(0, -0.12);
      else if (what === 'in') zoom(0.87); else if (what === 'out') zoom(1.15);
      else if (what === 'again') { view.held = false; view.zoom = 1; if (cur) view.el = cur.P.el; t = opt.calm ? END : 0; touch(); }
    }
    canvas.addEventListener('keydown', function (e) {
      var k = { ArrowLeft: 'left', ArrowRight: 'right', ArrowUp: 'up', ArrowDown: 'down', '+': 'in', '=': 'in', '-': 'out', r: 'again', R: 'again' }[e.key];
      if (k) { e.preventDefault(); act(k); }
    });
    canvas.addEventListener('webglcontextlost', function (e) { e.preventDefault(); alive = false; if (opt.lost) opt.lost(); });

    if (opt.calm) t = END;
    size(); wake();
    return {
      // a reading: the things, where they stand and what their labels say. The first one falls into place; a later
      // one is simply there, and the view stays as the reader left it
      set: function (T, P, L) { var first = !cur; build(T, P, L); if (!first && t > 0) t = Math.max(t, END); touch(); },
      act: act
    };
  }

  window.CountedScene = { mount: mount };
})();
