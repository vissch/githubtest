// Draws the house (house.js) on a canvas: the floors, the walls and what stands in the rooms are drawn here, in the
// plan's own isometric view; the frogs are the owner's sprites, packed by sprites.py and listed in data/frog.js
// (window.FROG). Without the sheets a frog is a mark of its kind, so the page still says who is where.
// Reads data/ops.js through crew.js every 20 seconds, as the office does. house.html?demo shows a made-up floor,
// &ff=40 runs it forty seconds on before the first picture, &room=lab opens on one room, &shot draws once and stops,
// &plates=light draws the rooms nobody is in as light plates too (to compare the two looks).
// On the control screen (index.html) the house stands beside the profile (board.js, docked into #profile): the
// profile is about the frog the owner clicked, else about the one the house follows, and a ring marks that frog.
(function () {
  'use strict';
  var C = window.Crew, H = window.House, F = window.FROG || {}, stage = document.getElementById('house');
  if (!C || !H || !stage) return;
  var el = C.el, cv = document.getElementById('h-canvas'), ctx = cv.getContext('2d');
  var tagsBox = document.getElementById('h-tags'), card = document.getElementById('h-card');
  var q = new URLSearchParams(location.search), demo = q.has('demo'), shot = q.has('shot');
  var still = !shot && window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var sim = new H.Sim(F), COS = H.COS, SIN = H.SIN, iso = H.iso;
  var DIMMEST = q.get('plates') === 'light' ? 0.1 : 0.86;         // how far a room nobody is awake in goes to the night colour

  // ---- colours: the site's own (kinetic.css): one orange-red accent with amber beside it, green for what is at
  // work. A room is a rounded plate: light where someone is awake, navy where nobody is, so the lit plates are
  // where the work is. What stands in a lit room is grey, white or ink, and each room has one thing in the accent;
  // a room nobody is awake in is drawn in thin blue lines on its navy plate. The night colour, that line and the
  // pond are the stage's (house.css, --h-night, --h-dimline, --h-pond), so the picture follows the page's look.
  var PLATE = [242, 242, 242], GREY = [230, 230, 230], EDGE = [205, 205, 205], SOFT = [154, 154, 154], WHITE = [255, 255, 255];
  var DARK = [18, 18, 18], DARK2 = [28, 28, 29], DARK3 = [42, 42, 44], FIRE = [255, 74, 28], FIRE2 = [255, 176, 58], WORK = [34, 197, 94], LEAF = [34, 160, 84];
  var look = getComputedStyle(stage);
  function shade(name, or) {          // "r, g, b" or #rrggbb
    var raw = look.getPropertyValue(name).trim(), hex = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(raw);
    var v = hex ? hex.slice(1).map(function (h) { return parseInt(h, 16); }) : raw.split(',').map(Number);
    return v.length === 3 && v.every(isFinite) ? v : or;
  }
  var NIGHT = shade('--h-night', DARK), DIMLINE = shade('--h-dimline', [154, 154, 154]), POND = shade('--h-pond', DARK2);
  var INK_LIT = 'rgba(18,18,18,.62)', INK = INK_LIT, LINE = 2.2, ROUND = 44, GAP = 7;      // a room's corners, and the dark between two rooms
  var LOOK = {
    lab: { floor: [244, 244, 244], line: [224, 224, 224], wall: [250, 250, 250] },
    shop: { floor: [230, 230, 230], line: [210, 210, 210], wall: [240, 240, 240] },
    studio: { floor: [237, 237, 237], line: [218, 218, 218], wall: [246, 246, 246] },
    hall: { floor: [250, 250, 250], line: [232, 232, 232], wall: [255, 255, 255] },
    bunk: { floor: [226, 226, 226], line: [206, 206, 206], wall: [236, 236, 236] },
    work: { floor: [241, 241, 241], line: [222, 222, 222], wall: [250, 250, 250] },
    plan: { floor: [233, 233, 233], line: [213, 213, 213], wall: [244, 244, 244] }
  };
  var KIND = { session: [34, 197, 94], skill: [59, 130, 246], agent: [168, 85, 247], machine: [150, 156, 168], role: [20, 184, 166] };
  var lit = {};
  H.ROOMS.forEach(function (r) { lit[r.id] = 1; });
  function mix(a, b, t) { return [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t]; }
  function css(c, a) { return 'rgba(' + (c[0] | 0) + ',' + (c[1] | 0) + ',' + (c[2] | 0) + ',' + (a == null ? 1 : a) + ')'; }
  function tone(c, room, k) {            // a colour as it looks in this room now; k lightens (+) or darkens (-)
    if (k) c = k > 0 ? mix(c, [255, 255, 255], k) : mix(c, [0, 0, 0], -k);
    return css(mix(c, NIGHT, Math.min(DIMMEST, (1 - (room ? lit[room] : 1)) * 1.7)));        // a room nobody is awake in is a navy plate
  }
  // is this room drawn as one nobody is awake in: its outlines are the thin blue line then, where a lit room's are ink
  function dimmed(room) { return DIMMEST > 0.5 && !!room && (1 - lit[room]) * 1.7 > 0.5; }
  function inkOf(room) { return dimmed(room) ? css(DIMLINE, 0.58) : INK_LIT; }

  // ---- the camera: the whole house, or one room
  var W = 0, Hh = 0, dpr = 1, cam = { x: 0, y: 0, s: 0.4 }, aim = { x: 0, y: 0, s: 0.4 }, focus = q.get('room') || '';
  function frame(room) {
    var b;
    if (room && H.ROOM[room]) { var r = H.ROOM[room], t = iso(r.u0, r.v0), l = iso(r.u0, r.v1), rr = iso(r.u1, r.v0), bt = iso(r.u1, r.v1);
      b = [l[0] - 60, t[1] - H.TALL - 70, rr[0] + 60, bt[1] + 50];
    } else b = [iso(0, H.V[3])[0] - 40, iso(H.U[1], 0)[1] - H.TALL - 46, iso(H.U[3], 0)[0] + 40, iso(H.U[3], H.V[2])[1] + 76];
    var s = Math.min(W / (b[2] - b[0]), Hh / (b[3] - b[1]));
    return { x: (b[0] + b[2]) / 2, y: (b[1] + b[3]) / 2, s: Math.min(s, 1.35) };
  }
  function size() {
    var r = stage.getBoundingClientRect();
    W = Math.max(320, r.width); Hh = Math.max(200, r.height); dpr = Math.min(2, window.devicePixelRatio || 1);
    cv.width = Math.round(W * dpr); cv.height = Math.round(Hh * dpr);
    aim = frame(focus); if (!sized || still) { cam = { x: aim.x, y: aim.y, s: aim.s }; sized = true; }
    dirty = true;
    if (still && ready) render();          // nothing else draws again when motion is off: a new size left the picture blank
  }
  var sized = false, dirty = true, ready = false;
  function toScreen(x, y) { return [(x - cam.x) * cam.s + W / 2, (y - cam.y) * cam.s + Hh / 2]; }
  function toScene(px, py) { return [(px - W / 2) / cam.s + cam.x, (py - Hh / 2) / cam.s + cam.y]; }

  // ---- the frogs' sheets, at full and at half size; the half one is drawn while a frog is small on the screen
  var sheets = {};
  function sheet(key, half) {
    var name = key + (half ? '@0.5' : ''), s = sheets[name];
    if (!s) { s = sheets[name] = { ok: false, el: new Image() }; s.el.onload = function () { s.ok = true; dirty = true; }; s.el.src = 'img/frog/' + name + '.png'; }
    return s;
  }
  var hasFrog = !!(F.anims && F.foot);
  // every clip is asked for at the start, at half size (under 2 MB in all): a sheet is otherwise first asked for when a
  // frog needs it, and until it came that frog was a mark: a green dot walked out of the bunkhouse in place of a frog
  if (hasFrog) Object.keys(F.anims).concat(Object.keys(F.stills || {})).forEach(function (k) { sheet(k, true); });
  function sprite(key, frameNo, u, v, z, flip, alpha) {
    var a = (F.anims && F.anims[key]) || (F.stills && F.stills[key]);
    if (!a) return null;
    var half = cam.s * dpr <= 0.56, img = sheet(key, half);
    if (!img.ok) { half = !half; img = sheet(key, half); if (!img.ok) return null; }
    var cw = half ? Math.round(a.w / 2) : a.w, ch = half ? Math.round(a.h / 2) : a.h, cols = a.cols || 1;
    var sx = (frameNo % cols) * cw, sy = Math.floor(frameNo / cols) * ch, p = iso(u, v, z), x = a.ox - F.foot[0], y = p[1] + a.oy - F.foot[1];
    ctx.save(); ctx.translate(p[0], 0); if (flip) ctx.scale(-1, 1); if (alpha != null) ctx.globalAlpha = alpha;
    ctx.drawImage(img.el, sx, sy, cw, ch, x, y, a.w, a.h);
    ctx.restore();
    return flip ? [p[0] - x - a.w, y, p[0] - x, y + a.h] : [p[0] + x, y, p[0] + x + a.w, y + a.h];
  }

  // ---- drawing on the plan
  function path(pts) { ctx.beginPath(); ctx.moveTo(pts[0][0], pts[0][1]); for (var i = 1; i < pts.length; i++) ctx.lineTo(pts[i][0], pts[i][1]); ctx.closePath(); }
  function poly(pts, fill, stroke, lw) { path(pts); if (fill) { ctx.fillStyle = fill; ctx.fill(); } if (stroke) { ctx.strokeStyle = stroke; ctx.lineWidth = lw || LINE; ctx.lineJoin = 'round'; ctx.stroke(); } }
  function box(u0, v0, u1, v1, z0, z1, c, room, o) {       // the three faces the eye sees: the top, the one down-left, the one down-right
    o = o || {};
    poly([iso(u0, v1, z0), iso(u1, v1, z0), iso(u1, v1, z1), iso(u0, v1, z1)], tone(o.left || c, room, -0.1), INK);
    poly([iso(u1, v0, z0), iso(u1, v1, z0), iso(u1, v1, z1), iso(u1, v0, z1)], tone(o.right || c, room, -0.22), INK);
    poly([iso(u0, v0, z1), iso(u1, v0, z1), iso(u1, v1, z1), iso(u0, v1, z1)], tone(o.top || c, room, o.top ? 0 : 0.12), INK);
  }
  function disc(u, v, r, z, fill, stroke) { var p = iso(u, v, z); ctx.beginPath(); ctx.ellipse(p[0], p[1], r * 1.2247, r * 0.7071, 0, 0, Math.PI * 2); if (fill) { ctx.fillStyle = fill; ctx.fill(); } if (stroke) { ctx.strokeStyle = stroke; ctx.lineWidth = LINE; ctx.stroke(); } }
  function drum(u, v, r, z0, z1, c, room) {
    var a = iso(u, v, z0), b = iso(u, v, z1), rx = r * 1.2247;
    ctx.beginPath(); ctx.ellipse(a[0], a[1], rx, r * 0.7071, 0, 0, Math.PI); ctx.lineTo(b[0] - rx, b[1]); ctx.lineTo(b[0] + rx, b[1]); ctx.closePath();
    ctx.fillStyle = tone(c, room, -0.16); ctx.fill(); ctx.strokeStyle = INK; ctx.lineWidth = LINE; ctx.stroke();
    disc(u, v, r, z1, tone(c, room, 0.1), INK);
  }
  function onFloor(fn) { ctx.save(); ctx.transform(COS, SIN, -COS, SIN, 0, 0); fn(); ctx.restore(); }                       // draw in (u, v): it lies on the floor
  function onWallV(b, fn) { ctx.save(); ctx.transform(COS, SIN, 0, 1, -b * COS, b * SIN); fn(); ctx.restore(); }             // a wall with v fixed: draw in (u, -z)
  function onWallU(a, fn) { ctx.save(); ctx.transform(COS, -SIN, 0, 1, a * COS, a * SIN); fn(); ctx.restore(); }             // a wall with u fixed: draw in (-v, -z), so words read left to right
  function shadow(u, v, rx, a) { var p = iso(u, v); ctx.beginPath(); ctx.ellipse(p[0], p[1], rx, rx * 0.5, 0, 0, Math.PI * 2); ctx.fillStyle = 'rgba(20,18,30,' + (a || 0.2) + ')'; ctx.fill(); }

  // ---- the floors, and what lies flat on them
  function floors(t) {
    H.ROOMS.forEach(function (r) {
      var L = LOOK[r.id], w = r.u1 - r.u0, d = r.v1 - r.v0;
      onFloor(function () {
        ctx.beginPath(); ctx.roundRect(r.u0 + GAP, r.v0 + GAP, w - 2 * GAP, d - 2 * GAP, ROUND); ctx.fillStyle = tone(L.floor, r.id); ctx.fill();
        var dim = dimmed(r.id), ln = dim ? css(mix(NIGHT, DIMLINE, 0.3)) : tone(L.line, r.id);
        ctx.strokeStyle = dim ? css(DIMLINE, 0.5) : 'rgba(255,255,255,.16)'; ctx.lineWidth = 3; ctx.stroke();          // the edge of a card: a dark plate still reads as a room
        ctx.save(); ctx.clip();
        ctx.fillStyle = ln; ctx.strokeStyle = ln; ctx.lineWidth = 2;
        var i, j;
        if (r.id === 'hall') { for (i = 0; i * 70 < w; i++) for (j = 0; j * 70 < d; j++) if ((i + j) % 2) ctx.fillRect(r.u0 + i * 70, r.v0 + j * 70, 70, 70); }
        else if (r.id === 'work') { for (j = 0; j * 46 < d; j++) { ctx.fillRect(r.u0, r.v0 + j * 46, w, 2); for (i = 0; i * 180 < w + 180; i++) ctx.fillRect(r.u0 + i * 180 - (j % 3) * 60, r.v0 + j * 46, 2, 46); } }
        else if (r.id === 'bunk') { for (i = 0; i * 190 < w; i++) for (j = 0; j * 95 < d; j++) ctx.strokeRect(r.u0 + i * 190 + (j % 2) * 95, r.v0 + j * 95, 190, 95); }
        else if (r.id === 'plan') { ctx.lineWidth = 10; ctx.strokeRect(r.u0 + 36, r.v0 + 36, w - 72, d - 72); }
        else if (r.id === 'studio') { for (i = 0; i * 56 < w; i++) ctx.fillRect(r.u0 + i * 56, r.v0, 2, d); }
        else { var g = r.id === 'lab' ? 70 : 140; for (i = 1; i * g < w; i++) ctx.fillRect(r.u0 + i * g, r.v0, 2, d); for (j = 1; j * g < d; j++) ctx.fillRect(r.u0, r.v0 + j * g, w, 2); }
        if (r.id === 'shop') { ctx.fillStyle = tone(FIRE2, r.id); for (i = 0; i < 28; i++) if (i % 2) ctx.fillRect(r.u0 + 60 + i * 22, r.v0 + 108, 22, 9); }
        ctx.restore();
      });
    });
    // the pond behind the hall and the path to the front gate
    onFloor(function () {
      ctx.fillStyle = 'rgba(255,255,255,.045)';
      ctx.beginPath(); ctx.roundRect(60 + GAP, 80, 500 - 2 * GAP, 560 - GAP, ROUND); ctx.fill();
      ctx.beginPath(); ctx.roundRect(1260 + GAP, 1160 + GAP, 760 - 2 * GAP, 330, ROUND); ctx.fill();
      ctx.fillStyle = 'rgba(242,242,242,.92)'; for (var i = 0; i < 9; i++) { ctx.beginPath(); ctx.roundRect(1330 + i * 76, 1186 + (i % 2) * 8, 56, 40, 20); ctx.fill(); }
    });
    disc(330, 330, 150, 0, css(POND, 0.96), css(DIMLINE, 0.4)); disc(330, 330, 118, 0, 'rgba(255,255,255,.06)');
    [[280, 300, 26], [390, 372, 20], [352, 262, 15]].forEach(function (p, i) { disc(p[0], p[1], p[2], 0, css(LEAF, 0.95), 'rgba(18,18,18,.5)'); if (!i) disc(p[0] + 6, p[1] - 4, 7, 4, css(FIRE)); });
    var rip = (t * 0.25) % 1; disc(330, 330, 40 + rip * 70, 0, null, 'rgba(242,242,242,' + (0.4 * (1 - rip)).toFixed(3) + ')');
    // the rug, the stage and the mats
    H.THINGS.forEach(function (th) {
      if (!th.over) return;
      var b = th.box, room = th.room;
      INK = inkOf(room);
      if (th.kind === 'rug') onFloor(function () {       // the accent of the hall: an orange card with the site's eight-point star
        var cu = (b[0] + b[2]) / 2, cv = (b[1] + b[3]) / 2, k;
        ctx.beginPath(); ctx.roundRect(b[0], b[1], b[2] - b[0], b[3] - b[1], 36); ctx.fillStyle = tone(FIRE, room); ctx.fill();
        ctx.beginPath(); ctx.roundRect(b[0] + 20, b[1] + 20, b[2] - b[0] - 40, b[3] - b[1] - 40, 22); ctx.strokeStyle = tone([255, 214, 200], room); ctx.lineWidth = 4; ctx.stroke();
        ctx.beginPath(); for (k = 0; k < 16; k++) { var rr = k % 2 ? 26 : 74, an = k * Math.PI / 8; ctx[k ? 'lineTo' : 'moveTo'](cu + Math.cos(an) * rr, cv + Math.sin(an) * rr); } ctx.closePath();
        ctx.fillStyle = tone(WHITE, room); ctx.fill();
      });
      if (th.kind === 'stage') { var d = th.disc; drum(d[0], d[1], d[2], 0, d[3], DARK2, room); disc(d[0], d[1], d[2] - 26, d[3], null, tone([110, 110, 112], room));
        disc(d[0], d[1], 54, d[3], tone(FIRE2, room)); }
      if (th.kind === 'mat') {
        box(b[0], b[1], b[2], b[3], 0, b[4], [236, 236, 236], room);
        var bl = [DARK3, [120, 120, 124], EDGE, FIRE][th.spot % 4];
        poly([iso(b[0] + 4, b[1] + 4, b[4] + 1), iso(b[0] + 92, b[1] + 4, b[4] + 1), iso(b[0] + 92, b[3] - 4, b[4] + 1), iso(b[0] + 4, b[3] - 4, b[4] + 1)], tone(bl, room), INK);
        box(b[2] - 46, b[1] + 9, b[2] - 9, b[3] - 9, b[4], b[4] + 8, [252, 250, 244], room);          // the pillow, at the head end
      }
    });
    INK = INK_LIT;
  }
  // where each room's name stands: over the middle of its back wall, or outside its front edge when it has none.
  // The hall's stands over its back rail, between the notice board and the doorway: over the board it covered the
  // board's number, and further right it stood in the doorway, over whoever walked through
  var NAMEAT = { lab: [280, 640, H.TALL, -1], shop: [910, 0, H.TALL, -1], studio: [0, 1480, H.TALL, -1], hall: [1040, 640, 26, -1], bunk: [1640, 0, H.TALL, -1],
                 work: [910, 1800, 0, 1], plan: [2020, 900, 0, 1] };

  // ---- the things that stand up: built once, in an order in which each is drawn after what is behind it
  var statics = [];
  function thing(u0, v0, u1, v1, h, room, draw) {
    var it = { u0: u0, v0: v0, u1: u1, v1: v1, h: h, room: room, draw: draw };
    it.x0 = (u0 - v1) * COS - 6; it.x1 = (u1 - v0) * COS + 6; it.y0 = (u0 + v0) * SIN - h - 6; it.y1 = (u1 + v1) * SIN + 6;
    statics.push(it); return it;
  }
  function wallRoom(w, a, b) { var m = (a + b) / 2; return w.fix === 'u' ? (H.roomAt(w.at + 20, m) || H.roomAt(w.at - 20, m)) : (H.roomAt(m, w.at + 20) || H.roomAt(m, w.at - 20)); }
  function wallFace(w, a, b, room) {      // the inside of a tall back wall, with what hangs on it
    var c = LOOK[room].wall, T = 12, h = w.h;
    if (w.fix === 'u') {
      poly([iso(w.at - T, a, h), iso(w.at, a, h), iso(w.at, b, h), iso(w.at - T, b, h)], tone(c, room, 0.2), INK);
      poly([iso(w.at - T, b, 0), iso(w.at, b, 0), iso(w.at, b, h), iso(w.at - T, b, h)], tone(c, room, -0.12), INK);
      poly([iso(w.at, a, 0), iso(w.at, b, 0), iso(w.at, b, h), iso(w.at, a, h)], tone(c, room, -0.05), INK);
      poly([iso(w.at, a, 0), iso(w.at, b, 0), iso(w.at, b, 22), iso(w.at, a, 22)], tone(c, room, -0.2));
    } else {
      poly([iso(a, w.at - T, h), iso(b, w.at - T, h), iso(b, w.at, h), iso(a, w.at, h)], tone(c, room, 0.2), INK);
      poly([iso(b, w.at - T, 0), iso(b, w.at, 0), iso(b, w.at, h), iso(b, w.at - T, h)], tone(c, room, -0.2), INK);
      poly([iso(a, w.at, 0), iso(b, w.at, 0), iso(b, w.at, h), iso(a, w.at, h)], tone(c, room, 0.04), INK);
      poly([iso(a, w.at, 0), iso(b, w.at, 0), iso(b, w.at, 22), iso(a, w.at, 22)], tone(c, room, -0.14));
    }
  }
  function rect(x, y, w, h, fill, stroke) { if (fill) { ctx.fillStyle = fill; ctx.fillRect(x, y, w, h); } if (stroke) { ctx.strokeStyle = stroke; ctx.lineWidth = LINE; ctx.strokeRect(x, y, w, h); } }
  // what hangs on the tall walls, a room at a time. Drawn in the wall's own plane: x along the wall, y down from the floor line.
  var HANG = {
    'u0:lab': function () {
      onWallU(0, function () {         // shelves of jars
        for (var s = 0; s < 2; s++) { rect(-1120, -150 + s * 62, 420, 7, tone(SOFT, 'lab'), INK);
          for (var i = 0; i < 9; i++) rect(-1108 + i * 46, -150 + s * 62 - 30 - (i * 7 + s * 5) % 12, 24, 30 + (i * 7 + s * 5) % 12, tone([FIRE, DARK2, FIRE2, EDGE][(i + s) % 4], 'lab'), INK); }
      });
    },
    'v640:lab': function (t) {
      onWallV(640, function () {       // a chart and a window
        rect(60, -152, 190, 104, tone(DARK2, 'lab'), INK);
        ctx.strokeStyle = tone(FIRE, 'lab'); ctx.lineWidth = 4; ctx.beginPath(); ctx.moveTo(76, -70);
        for (var i = 0; i < 9; i++) ctx.lineTo(76 + i * 20, -70 - 22 * Math.sin(i * 0.9 + t * 0.6) - i * 4); ctx.stroke();
        rect(330, -150, 150, 96, tone([214, 218, 222], 'lab', 0.1), INK); rect(403, -150, 4, 96, tone(WHITE, 'lab'));
      });
    },
    'u0:studio': function () {
      onWallU(0, function () {         // the green screen, and a clapper on the wall
        rect(-1700, -160, 380, 150, tone(WORK, 'studio'), INK);
        rect(-1280, -150, 80, 60, tone([36, 36, 44], 'studio'), INK);
        for (var i = 0; i < 4; i++) rect(-1280 + i * 20, -150, 10, 16, tone([240, 240, 240], 'studio'));
      });
    },
    'u560:shop': function () {
      onWallU(560, function () {       // a board of tools
        rect(-600, -156, 250, 104, tone(DARK2, 'shop'), INK);
        ctx.strokeStyle = tone(GREY, 'shop'); ctx.lineWidth = 7; ctx.lineCap = 'round';
        [[-570, -140, -570, -80], [-540, -140, -520, -84], [-496, -136, -496, -92], [-470, -140, -440, -140], [-455, -140, -455, -86], [-420, -136, -400, -96], [-384, -140, -384, -76]].forEach(function (l) {
          ctx.beginPath(); ctx.moveTo(l[0], l[1]); ctx.lineTo(l[2], l[3]); ctx.stroke(); });
        rect(-300, -150, 110, 70, tone(FIRE, 'shop'), INK);
        ctx.fillStyle = tone(WHITE, 'shop'); ctx.font = '700 30px Inter, sans-serif'; ctx.fillText('SHOP', -288, -102);
      });
    },
    'v0:shop': function () {
      onWallV(0, function () {         // a pipe along the wall
        rect(580, -160, 670, 12, tone(SOFT, 'shop'), INK);
        for (var i = 0; i < 4; i++) rect(640 + i * 170, -164, 12, 20, tone([110, 110, 112], 'shop'), INK);
      });
    },
    'v0:bunk': function (t) {
      onWallV(0, function () {         // three windows on the night
        for (var i = 0; i < 3; i++) { var x = 1340 + i * 230;
          rect(x, -156, 150, 104, css(mix([10, 13, 22], [50, 58, 82], 0.3 + 0.2 * lit.bunk)), INK);
          ctx.fillStyle = 'rgba(255,255,255,.85)'; for (var k = 0; k < 6; k++) { var tw = 0.5 + 0.5 * Math.sin(t * 1.3 + k * 2.1 + i); ctx.globalAlpha = 0.35 + 0.65 * tw; ctx.fillRect(x + 12 + (k * 53 + i * 31) % 126, -146 + (k * 37 + i * 17) % 80, 3, 3); }
          ctx.globalAlpha = 1; rect(x + 73, -156, 4, 104, tone(GREY, 'bunk')); }
        ctx.fillStyle = css(FIRE2, 0.95); ctx.beginPath(); ctx.arc(1378, -124, 15, 0, Math.PI * 2); ctx.fill();
        ctx.fillStyle = css(mix([10, 13, 22], [50, 58, 82], 0.3 + 0.2 * lit.bunk)); ctx.beginPath(); ctx.arc(1386, -128, 13, 0, Math.PI * 2); ctx.fill();
      });
    }
  };
  var TOOLS = {
    bench: function (b, room, th, t) {
      box(b[0], b[1], b[2], b[3], 0, b[4] - 6, EDGE, room); box(b[0] - 4, b[1] - 4, b[2] + 4, b[3] + 4, b[4] - 6, b[4], WHITE, room);
      var u = (b[0] + b[2]) / 2;
      [760, 910, 1060].forEach(function (v, i) {       // one thing to look at in front of each place at the bench
        var k = (i + (b[0] > 200 ? 1 : 0)) % 3;
        if (k === 0) { drum(u, v, 13, b[4], b[4] + 26, GREY, room); disc(u, v, 5, b[4] + 14, tone(FIRE, room)); }
        else if (k === 1) { box(u - 16, v - 12, u + 16, v + 12, b[4], b[4] + 5, DARK3, room); drum(u, v, 6, b[4] + 5, b[4] + 34, DARK2, room); }
        else { box(u - 17, v - 20, u + 17, v + 20, b[4], b[4] + 4, GREY, room); disc(u, v, 6, b[4] + 5, tone(FIRE, room)); }
        drum(u + 4, v - 68, 8, b[4], b[4] + 18 + 8 * i, [DARK2, FIRE, FIRE2][i], room);
      });
    },
    machine: function (b, room, th, t, busy) {
      var front = b[1] < 120, body = [DARK2, FIRE, SOFT, GREY, DARK3, EDGE][th.look], h = b[4];
      box(b[0], b[1], b[2], b[3], 0, h, body, room);
      var on = busy ? 1 : 0.35, blink = function (i) { return 0.35 + 0.65 * (Math.sin(t * (2 + i % 3) + i * 1.7) > 0 ? 1 : 0.2) * on; };
      var draw = function () {
        var x0 = front ? b[0] : -b[3], wd = front ? b[2] - b[0] : b[3] - b[1], i;
        if (th.look === 0 || th.look === 4) { for (i = 0; i < 4; i++) { rect(x0 + 12, -h + 14 + i * 22, wd - 24, 14, tone([8, 8, 8], room), INK);
          ctx.fillStyle = css(mix([30, 50, 38], WORK, blink(i))); ctx.fillRect(x0 + 18, -h + 18 + i * 22, 8, 6);
          ctx.fillStyle = css(mix([60, 44, 30], FIRE2, blink(i + 5))); ctx.fillRect(x0 + 32, -h + 18 + i * 22, 8, 6); } }
        else if (th.look === 1 || th.look === 5) { for (i = 0; i < 3; i++) rect(x0 + 10, -h + 16 + i * 26, wd - 20, 20, tone(body, room, 0.16), INK);
          for (i = 0; i < 3; i++) rect(x0 + wd / 2 - 12, -h + 23 + i * 26, 24, 5, tone([240, 240, 236], room)); }
        else { ctx.beginPath(); ctx.arc(x0 + wd / 2, -h * 0.58, Math.min(wd, h) * 0.28, 0, Math.PI * 2); ctx.fillStyle = tone([236, 238, 232], room); ctx.fill(); ctx.strokeStyle = INK; ctx.lineWidth = LINE; ctx.stroke();
          var ang = busy ? t * 2.4 : -0.6 + 0.2 * Math.sin(t); ctx.beginPath(); ctx.moveTo(x0 + wd / 2, -h * 0.58); ctx.lineTo(x0 + wd / 2 + Math.cos(ang) * 20, -h * 0.58 + Math.sin(ang) * 20);
          ctx.strokeStyle = tone(FIRE, room); ctx.lineWidth = 4; ctx.stroke(); }
      };
      if (front) onWallV(b[3], draw); else onWallU(b[2], draw);
      if (th.look === 2) drum((b[0] + b[2]) / 2 + 20, (b[1] + b[3]) / 2, 13, h, h + 44, [70, 70, 72], room);
    },
    workbench: function (b, room) {
      box(b[0] + 8, b[1] + 8, b[2] - 8, b[3] - 8, 0, b[4] - 8, DARK3, room); box(b[0], b[1], b[2], b[3], b[4] - 8, b[4], WHITE, room);
      box(b[0] + 24, b[1] + 14, b[0] + 66, b[1] + 44, b[4], b[4] + 22, FIRE, room);
      box(b[2] - 80, b[1] + 16, b[2] - 34, b[1] + 40, b[4], b[4] + 12, DARK2, room); drum(b[2] - 120, b[1] + 34, 12, b[4], b[4] + 9, FIRE2, room);
    },
    lamp: function (b, room, th, t) {
      var u = (b[0] + b[2]) / 2, v = (b[1] + b[3]) / 2, a = iso(u, v), top = iso(u, v, b[4]);
      ctx.strokeStyle = tone([60, 60, 70], room); ctx.lineWidth = 5; ctx.beginPath(); ctx.moveTo(a[0], a[1]); ctx.lineTo(top[0], top[1]); ctx.moveTo(a[0] - 16, a[1] + 8); ctx.lineTo(a[0] + 16, a[1] + 8); ctx.stroke();
      if (lit.studio > 0.7) { var s = iso(290, 1480); ctx.beginPath(); ctx.moveTo(top[0], top[1]); ctx.lineTo(s[0] - 150, s[1] - 10); ctx.lineTo(s[0] + 150, s[1] + 30); ctx.closePath(); ctx.fillStyle = 'rgba(255,240,190,.13)'; ctx.fill(); }
      box(u - 14, v - 14, u + 20, v + 14, b[4] - 12, b[4] + 16, [50, 52, 64], room, { right: [255, 236, 170] });
    },
    camera: function (b, room) {
      var u = (b[0] + b[2]) / 2, v = (b[1] + b[3]) / 2, top = iso(u, v, 70);
      ctx.strokeStyle = tone([60, 60, 70], room); ctx.lineWidth = 5; ctx.beginPath();
      [[-26, 10], [26, 10], [0, -18]].forEach(function (d) { var f = iso(u + d[0], v + d[1]); ctx.moveTo(top[0], top[1]); ctx.lineTo(f[0], f[1]); }); ctx.stroke();
      box(u - 20, v - 14, u + 20, v + 14, 70, b[4], [46, 48, 58], room); box(u - 34, v - 8, u - 20, v + 8, 76, 92, [30, 30, 38], room);
      drum(u + 6, v + 2, 9, b[4], b[4] + 18, [70, 72, 84], room);
    },
    board: function (b, room) {        // the notice board: what waits on the owner
      box(b[0], b[1], b[2], b[3], 0, b[4], DARK2, room, { left: DARK3 });
      onWallV(b[3], function () {
        var n = window.OwnerQueue && window.QUEUE ? window.OwnerQueue.count(window.QUEUE) : 0;
        rect(b[0] + 12, -b[4] + 12, b[2] - b[0] - 24, b[4] - 46, tone(PLATE, room), INK);
        ctx.fillStyle = tone(n ? [214, 52, 12] : LEAF, room); ctx.font = '700 22px Inter, sans-serif'; ctx.textBaseline = 'alphabetic'; ctx.fillText(n ? 'NEEDS YOU' : 'ALL CLEAR', b[0] + 24, -b[4] + 42);
        ctx.font = '700 58px Inter, sans-serif'; ctx.fillText(String(n || ''), b[0] + 24, -b[4] + 98);
        for (var i = 0; i < Math.min(8, n); i++) rect(b[0] + 110 + (i % 4) * 28, -b[4] + 54 + Math.floor(i / 4) * 28, 22, 22, tone([FIRE2, FIRE, DARK2, EDGE][i % 4], room), INK);
      });
    },
    plant: function (b, room, th, t) {
      var u = (b[0] + b[2]) / 2, v = (b[1] + b[3]) / 2;
      drum(u, v, 15, 0, 26, DARK2, room);
      for (var i = 0; i < 5; i++) { var p = iso(u + Math.cos(i * 1.26) * 10, v + Math.sin(i * 1.26) * 10, 44 + (i % 2) * 14), sw = Math.sin(t * 0.8 + i) * 1.5;
        ctx.beginPath(); ctx.ellipse(p[0] + sw, p[1], 15, 22, (i - 2) * 0.35, 0, Math.PI * 2); ctx.fillStyle = tone([LEAF, WORK][i % 2], room); ctx.fill(); ctx.strokeStyle = INK; ctx.lineWidth = LINE; ctx.stroke(); }
    },
    table: function (b, room) {
      box(b[0] + 16, b[1] + 16, b[2] - 16, b[3] - 16, 0, b[4] - 8, DARK3, room); box(b[0], b[1], b[2], b[3], b[4] - 8, b[4], DARK2, room);
      var p = function (u, v) { return iso(u, v, b[4] + 1); };
      poly([p(b[0] + 30, b[1] + 22), p(b[2] - 30, b[1] + 22), p(b[2] - 30, b[3] - 22), p(b[0] + 30, b[3] - 22)], tone(PLATE, room), INK);
      poly([p(b[0] + 60, b[1] + 40), p(b[0] + 150, b[1] + 34), p(b[0] + 170, b[3] - 40), p(b[0] + 70, b[3] - 34)], tone(EDGE, room));
      poly([p(b[0] + 190, b[1] + 44), p(b[2] - 50, b[1] + 38), p(b[2] - 60, b[3] - 36), p(b[0] + 200, b[3] - 44)], tone([222, 222, 222], room));
      [[90, 60, FIRE], [220, 90, DARK2], [150, 104, FIRE]].forEach(function (m) { drum(b[0] + m[0], b[1] + m[1], 5, b[4], b[4] + 12, m[2], room); });
    },
    easel: function (b, room) {
      box(b[0], b[1], b[2], b[3], 0, b[4], GREY, room, { right: WHITE });
      onWallU(b[2], function () {
        ctx.strokeStyle = tone(FIRE, room); ctx.lineWidth = 4; ctx.beginPath(); ctx.moveTo(-b[3] + 10, -30); ctx.lineTo(-b[3] + 34, -64); ctx.lineTo(-b[3] + 56, -52); ctx.lineTo(-b[3] + 86, -108); ctx.stroke();
        for (var i = 0; i < 3; i++) rect(-b[3] + 12 + i * 28, -130, 20, 14, tone([FIRE2, FIRE, EDGE][i], room), INK);
      });
    },
    desk: function (b, room, th, t) {
      var s = H.SPOTS.work[th.spot], who = seated[th.spot], k = iso(s.u - 40, s.v - 16);
      ctx.beginPath(); ctx.ellipse(k[0], k[1] + 4, 104, 50, 0, 0, Math.PI * 2); ctx.fillStyle = 'rgba(40,30,20,.13)'; ctx.fill();
      var drew = who ? sprite('office', who.pose.frame, s.u, s.v, 0, false) : sprite('desk', 0, s.u, s.v, 0, false, 0.92);
      if (who) { var c = iso(s.u, s.v); who.box = [c[0] - 92, c[1] - 186, c[0] + 22, c[1] + 6]; }
      if (!drew) { box(s.u - 90, s.v - 86, s.u + 14, s.v - 28, 0, 62, GREY, room); box(s.u - 62, s.v - 4, s.u - 14, s.v + 38, 0, 34, DARK3, room); if (who) mark(who, s.u - 38, s.v + 16, 40); }
    }
  };
  var seated = {};
  (function build() {
    H.WALLS.forEach(function (w) {
      var cuts = [w.from].concat((w.gaps || []).reduce(function (a, g) { return a.concat(g); }, []), [w.to]);
      for (var i = 0; i < cuts.length; i += 2) (function (a, b) {
        if (b - a < 1) return;
        if (w.h === H.TALL) {     // a back wall: a piece per room, so each dims with its room
          var parts = w.fix === 'u' ? H.V : H.U, at = [a].concat(parts.filter(function (x) { return x > a && x < b; }), [b]);
          for (var k = 1; k < at.length; k++) (function (p, n) {
            var room = wallRoom(w, p, n), hang = HANG[w.fix + w.at + ':' + room];
            thing(w.fix === 'u' ? w.at - 12 : p, w.fix === 'u' ? p : w.at - 12, w.fix === 'u' ? w.at : n, w.fix === 'u' ? n : w.at, w.h, room, function (t) {
              wallFace(w, p, n, room); if (hang) hang(t); });
          })(at[k - 1], at[k]);
        } else {
          var room = wallRoom(w, a, b) || 'hall', c = w.h < 20 ? EDGE : PLATE;
          if (w.fix === 'u') thing(w.at - 5, a, w.at + 5, b, w.h, room, function () { box(w.at - 5, a, w.at + 5, b, 0, w.h, c, room); });
          else thing(a, w.at - 5, b, w.at + 5, w.h, room, function () { box(a, w.at - 5, b, w.at + 5, 0, w.h, c, room); });
        }
      })(cuts[i], cuts[i + 1]);
    });
    H.THINGS.forEach(function (th) {
      if (th.over || !TOOLS[th.kind]) return;
      var b = th.box;
      thing(b[0], b[1], b[2], b[3], th.kind === 'desk' ? 150 : b[4] + 60, th.room, function (t) { TOOLS[th.kind](b, th.room, th, t, busyAt[th.room]); }).th = th;
    });
    // the order: a thing is behind another when it ends before the other begins along either axis, and they overlap on the screen
    var n = statics.length, before = statics.map(function () { return 0; }), after = statics.map(function () { return []; }), i, j;
    function behind(A, B) { return (A.u1 <= B.u0 + 1 || A.v1 <= B.v0 + 1) && !(B.u1 <= A.u0 + 1 || B.v1 <= A.v0 + 1); }
    for (i = 0; i < n; i++) for (j = 0; j < n; j++) {
      var A = statics[i], B = statics[j];
      if (i !== j && A.x0 < B.x1 && B.x0 < A.x1 && A.y0 < B.y1 && B.y0 < A.y1 && behind(A, B)) { after[i].push(j); before[j]++; }
    }
    var order = [], free = [];
    for (i = 0; i < n; i++) if (!before[i]) free.push(i);
    while (order.length < n) {
      if (!free.length) { for (i = 0; i < n; i++) if (before[i] > 0) { before[i] = 0; free.push(i); break; } }      // a ring of things each behind the next: break it
      free.sort(function (a, b) { return (statics[b].u0 + statics[b].v0) - (statics[a].u0 + statics[a].v0); });
      var x = free.pop(); order.push(x); before[x] = -1;
      after[x].forEach(function (y) { if (before[y] > 0 && --before[y] === 0) free.push(y); });
    }
    statics = order.map(function (i) { return statics[i]; });
  })();
  var busyAt = {};

  // ---- a frog
  function mark(f, u, v, r) {          // no sheet for it: the mark of its kind
    var p = iso(u, v, r), c = KIND[f.w.kind] || KIND.role;
    ctx.beginPath(); ctx.arc(p[0], p[1], r, 0, Math.PI * 2); ctx.fillStyle = css(c); ctx.fill(); ctx.strokeStyle = INK; ctx.lineWidth = LINE; ctx.stroke();
    var g = new Path2D(C.GLYPH[C.GLYPH[f.w.id] ? f.w.id : f.w.kind] || C.GLYPH.skill), k = r / 14;
    ctx.save(); ctx.translate(p[0] - 12 * k, p[1] - 12 * k); ctx.scale(k, k); ctx.strokeStyle = '#fff'; ctx.lineWidth = 1.8; ctx.lineCap = 'round'; ctx.lineJoin = 'round'; ctx.stroke(g); ctx.restore();
    return [p[0] - r, p[1] - r, p[0] + r, p[1] + r];
  }
  function frog(f, t) {
    var p = f.pose, u = p.u + (f.off || 0), v = p.v + (f.off || 0), z = p.lying ? 8 : 0, b;
    if (!p.lying) shadow(u, v, 36, 0.2);
    b = p.anim ? sprite(p.anim, p.frame, u, v, z, p.flip) : null;
    var c = iso(u, v);
    if (!b) f.box = mark(f, u, v, p.lying ? 26 : 34);
    else f.box = p.lying ? [c[0] - 70, c[1] - 88, c[0] + 76, c[1] + 12] : H.frogBox(u, v);
    if (f.st === 'sleep') zzz(u, v, t, f.n);
    if (f.w.wait === 'owner' && f.st !== 'walk') bubble(u, v, '?');
  }
  function zzz(u, v, t, n) {
    var p = iso(u + 40, v - 10, 70);
    ctx.font = '700 22px Inter, sans-serif'; ctx.textBaseline = 'alphabetic';
    for (var i = 0; i < 3; i++) { var k = ((t * 0.28 + i / 3 + (n % 13) / 13) % 1); ctx.fillStyle = 'rgba(242,242,242,' + (0.85 * Math.sin(k * Math.PI)).toFixed(3) + ')'; ctx.fillText(i === 2 ? 'Z' : 'z', p[0] + k * 26 + i * 4, p[1] - k * 52); }
  }
  function bubble(u, v, text) {
    var p = iso(u + 26, v - 26, 168);
    ctx.beginPath(); ctx.arc(p[0], p[1], 24, 0, Math.PI * 2); ctx.fillStyle = '#ff4a1c'; ctx.fill(); ctx.strokeStyle = INK; ctx.lineWidth = LINE; ctx.stroke();
    ctx.fillStyle = '#fff'; ctx.font = '700 32px Inter, sans-serif'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle'; ctx.fillText(text, p[0], p[1] + 2); ctx.textAlign = 'left'; ctx.textBaseline = 'alphabetic';
  }

  // ---- one picture
  var list = [], hover = null, pinned = null;
  function render() {
    var t = sim.t, i;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0); ctx.clearRect(0, 0, W, Hh);
    ctx.translate(W / 2, Hh / 2); ctx.scale(cam.s, cam.s); ctx.translate(-cam.x, -cam.y);
    ctx.imageSmoothingEnabled = true; ctx.imageSmoothingQuality = 'high';
    list = Object.keys(sim.frogs).map(function (k) { var f = sim.frogs[k]; f.pose = sim.pose(f); return f; });
    seated = {}; busyAt = {};
    var shared = {};
    list.forEach(function (f) {
      f.off = 0;
      if (f.at) { var k = f.at.room + f.at.i; f.off = (shared[k] || 0) * 26; shared[k] = (shared[k] || 0) + 1; if (f.at.room !== 'bunk') busyAt[f.at.room] = true; }
      if (f.pose.seated && f.st === 'at' && !f.off) seated[f.at.i] = f;
    });
    floors(t);
    var sh = shown(); if (sh && !sim.frogs[sh.key]) sh = null;
    if (sh && sh.box) ring(sh, t, false); else ringAt = null;
    // each frog on its feet goes in after the last thing that stands behind it
    var slots = statics.map(function () { return []; }), first = [];
    list.forEach(function (f) {
      if (seated[f.at && f.at.i] === f) return;
      var u = f.pose.u + f.off, v = f.pose.v + f.off, p = iso(u, v), at = -1;
      for (var k = 0; k < statics.length; k++) { var s = statics[k]; if (s.x0 < p[0] + 100 && s.x1 > p[0] - 100 && s.y0 < p[1] + 16 && s.y1 > p[1] - 190 && (s.u1 <= u + 2 || s.v1 <= v + 2)) at = k; }
      f.depth = u + v; (at < 0 ? first : slots[at]).push(f);
    });
    function line(fs) { fs.sort(function (a, b) { return a.depth - b.depth; }).forEach(function (f) { frog(f, t); }); }
    line(first);
    for (i = 0; i < statics.length; i++) { INK = inkOf(statics[i].room); statics[i].draw(t); INK = INK_LIT; line(slots[i]); }
    if (sh && sh.box) ring(sh, t, true);
    tags();
  }
  // the ring on the floor under whoever the panel is about: orange for the frog the owner picked, pale for the one
  // the house follows. It slides over to a newly chosen frog and breathes. Drawn twice: filled under the frog, and
  // its line again over everything, faint, so a desk in front does not hide who is meant.
  var ringAt = null, ringFor = null, ringT = 0;
  function ring(f, t, over) {
    var b = f.box, x = (b[0] + b[2]) / 2, y = f.pose && f.pose.lying ? b[3] - 30 : b[3] - 10, own = f === pinned;
    if (!over) {
      var k = still || !ringAt ? 1 : 0.2; ringAt = ringAt || { x: x, y: y }; ringAt.x += (x - ringAt.x) * k; ringAt.y += (y - ringAt.y) * k;
      if (ringFor !== f.key + own) { ringFor = f.key + own; ringT = t; }
    }
    if (!ringAt) return;
    var beat = still || t - ringT > 3 ? 0 : Math.sin((t - ringT) * 4.2) * (1 - (t - ringT) / 3), r = 70 + beat * 7,
      at = sim.frogs[f.key] && sim.frogs[f.key].at, ink = at && at.room === 'hall' ? '18,18,18' : '255,74,28';          // on the hall's orange rug the orange ring is not there: ink
    ctx.save(); ctx.translate(ringAt.x, ringAt.y); ctx.scale(1, 0.5);
    ctx.beginPath(); ctx.arc(0, 0, r, 0, Math.PI * 2);
    if (!over) { ctx.fillStyle = 'rgba(' + ink + ',' + (own ? 0.24 : 0.12) + ')'; ctx.fill(); }
    if (!own) ctx.setLineDash([26, 16]);
    ctx.lineWidth = Math.max(6, 3.2 / cam.s); ctx.strokeStyle = 'rgba(' + ink + ',' + (over ? 0.4 : 1) + ')'; ctx.stroke(); ctx.setLineDash([]);
    if (!over && beat) { ctx.beginPath(); ctx.arc(0, 0, r + 26 + beat * 12, 0, Math.PI * 2); ctx.lineWidth = Math.max(2, 1.4 / cam.s); ctx.strokeStyle = 'rgba(' + ink + ',' + (0.4 * Math.abs(beat)).toFixed(3) + ')'; ctx.stroke(); }
    ctx.restore();
  }

  // ---- names over the frogs (the page's own elements, so they stay sharp and can be clicked)
  var tagOf = {}, roomTag = {};
  function tags() {
    var want = {}, placed = [], n = sim.census(), chosen = shown();
    // what the notice board says is kept free: no name of a room or of a frog is put over it
    var face = H.boardFace(), fa = toScreen(face[0], face[1]), fb = toScreen(face[2], face[3]);
    placed.push([fa[0], fa[1], fb[0], fb[1]]);
    H.ROOMS.forEach(function (r) {
      var e = roomTag[r.id], a = NAMEAT[r.id];
      if (!e) { e = roomTag[r.id] = el('button', 'h-room h-' + r.id); e.type = 'button'; e.title = r.name + ': ' + r.does; e.appendChild(el('b', null, r.name)); e.appendChild(el('span')); tagsBox.appendChild(e);
        e.addEventListener('click', function () { go(r.id); }); }
      var count = String(n[r.id].here);
      if (e.dataset.n !== count) { e.dataset.n = count; e.lastChild.textContent = count; e.dataset.w = e.offsetWidth; e.dataset.h = e.offsetHeight; }
      e.classList.toggle('lit', n[r.id].awake > 0); e.classList.toggle('on', focus === r.id); e.setAttribute('aria-pressed', focus === r.id ? 'true' : 'false');
      var p = toScreen.apply(null, iso(a[0], a[1], a[2])), w = +e.dataset.w || 90, h = +e.dataset.h || 26, x = p[0] - w / 2, y = a[3] < 0 ? p[1] - h - 8 : p[1] + 10;
      var off = x < -w || x > W || y < -h || y > Hh; e.hidden = off; if (off) return;
      x = Math.max(4, Math.min(W - w - 4, x)); y = Math.max(4, Math.min(Hh - h - 4, y));
      placed.push([x, y, x + w, y + h]); e.style.transform = 'translate(' + Math.round(x) + 'px,' + Math.round(y) + 'px)';
    });
    // whoever the panel is about is placed first, so its name is never the one that gives way
    list.slice().sort(function (a, b) { return (b === chosen) - (a === chosen) || (b.box ? b.box[3] : 0) - (a.box ? a.box[3] : 0); }).forEach(function (f) {
      // on a phone a room fills the picture and the names of whoever shows at its edges covered it: there, only the room's own
      var awake = f.st !== 'sleep' && f.st !== 'down', near = W >= 560 || !focus || (f.at ? f.at.room : f.goal && f.goal.room) === focus;
      var show = f.box && (f === hover || f === chosen || (awake && near && cam.s >= 0.26) || (focus === 'bunk' && cam.s > 0.7));
      if (!show) return;
      var e = tagOf[f.key];
      if (!e) { e = tagOf[f.key] = el('button', 'h-tag'); e.type = 'button'; e.appendChild(el('b')); e.appendChild(el('span')); tagsBox.appendChild(e); e.addEventListener('click', function () { pin(sim.frogs[f.key]); }); }
      var name = C.title(f.w), sub = f.st === 'walk' ? '→ ' + (f.leaving ? 'home' : H.ROOM[f.goal.room].name) : (C.doing(f.w) || (f.w.kind === 'session' ? '' : f.w.what) || '');
      if (e.dataset.sig !== name + '|' + sub + '|' + f.w.kind) { e.dataset.sig = name + '|' + sub + '|' + f.w.kind; e.firstChild.textContent = name; e.lastChild.textContent = sub; e.className = 'h-tag k-' + f.w.kind + (sub ? '' : ' h-bare'); e.hidden = false; e.dataset.w = e.offsetWidth; e.dataset.h = e.offsetHeight; }
      e.classList.toggle('h-walk', f.st === 'walk'); e.classList.toggle('h-on', f === hover || f === pinned); e.classList.toggle('h-shown', f === chosen);
      e.setAttribute('aria-pressed', f === pinned ? 'true' : 'false');
      var p = toScreen((f.box[0] + f.box[2]) / 2, f.box[1]), w = +e.dataset.w || 80, h = +e.dataset.h || 30, x = p[0] - w / 2, y = p[1] - h - 4, tries = 0, hit = true;
      while (hit && tries++ < 8) { hit = false; for (var k = 0; k < placed.length; k++) { var r = placed[k]; if (x < r[2] + 4 && x + w + 4 > r[0] && y < r[3] + 2 && y + h + 2 > r[1]) { y = r[1] - h - 3; hit = true; } } }
      // a name pushed off the top by the others has no place: it is left out (the list and a hover still say it),
      // where it used to be put back on top of them
      if (y < 4 && f !== chosen && f !== hover) return;
      // the chosen one pushed off the top goes under its frog: clamped back to the top it lay over the room's name
      if (y < 4) y = Math.min(Hh - h - 4, toScreen(0, f.box[3])[1] + 6);
      x = Math.max(4, Math.min(W - w - 4, x)); y = Math.max(4, y);
      placed.push([x, y, x + w, y + h]); want[f.key] = true;
      e.style.transform = 'translate(' + Math.round(x) + 'px,' + Math.round(y) + 'px)'; e.hidden = false;
    });
    Object.keys(tagOf).forEach(function (k) { if (!want[k]) { if (sim.frogs[k]) tagOf[k].hidden = true; else { tagOf[k].remove(); delete tagOf[k]; } } });
  }

  // ---- who is under the pointer, and the card that says what it is at
  function at(px, py) { var s = toScene(px, py); for (var i = list.length - 1; i >= 0; i--) { var b = list[i].box; if (b && s[0] >= b[0] && s[0] <= b[2] && s[1] >= b[1] && s[1] <= b[3]) return list[i]; } return null; }
  function roomUnder(px, py) { var s = toScene(px, py), p = H.plan(s[0], s[1]); return H.roomAt(p[0], p[1]); }
  function showCard(f) {
    if (!f || (f === pinned && f !== hover && window.Board)) { card.hidden = true; return; }      // a pinned frog has the panel: the card would say it twice
    if (card.dataset.key !== f.key + '|' + (f.w.doing || '') + '|' + f.st) {
      card.dataset.key = f.key + '|' + (f.w.doing || '') + '|' + f.st; card.innerHTML = '';
      card.appendChild(C.booth(f.w));
      var tx = el('div', 'h-card-text'), room = f.at ? f.at.room : f.goal ? f.goal.room : '';
      tx.appendChild(el('b', null, C.title(f.w)));
      var state = f.leaving ? 'leaving' : f.st === 'walk' ? 'on the way to the ' + H.ROOM[room].name.toLowerCase() : f.w.wait === 'owner' ? 'waiting on you' : f.w.state === 'working' ? 'in the ' + H.ROOM[room].name.toLowerCase() : f.w.kind === 'session' ? 'resting, ' + C.ago(f.w.age || 0) : 'asleep, nothing calls it';
      tx.appendChild(el('span', 'h-card-state', state));
      var doing = C.doing(f.w); if (doing) tx.appendChild(el('span', 'h-card-doing', doing));
      if (f.w.what && f.w.what !== doing) tx.appendChild(el('span', 'h-card-what', f.w.what));
      if (f.where) tx.appendChild(el('span', 'h-card-where', f.where.replace(/^lane\/(show|sim)\//, '')));
      card.appendChild(tx);
    }
    var p = toScreen((f.box[0] + f.box[2]) / 2, f.box[3]), w = 300;
    card.style.left = Math.max(8, Math.min(W - w - 8, p[0] - w / 2)) + 'px';
    if (p[1] + 150 < Hh) { card.style.top = (p[1] + 10) + 'px'; card.style.bottom = 'auto'; } else { card.style.top = 'auto'; card.style.bottom = (Hh - toScreen(0, f.box[1])[1] + 34) + 'px'; }
    card.hidden = false;
  }
  // what the panel says about a frog: its branch, the models that branch touches, the last picture or film it had in
  // its hands, and the notes on any of it. A link stays on the page when what it leads to is on this page.
  function here(id, away) { return document.getElementById(id) ? '#' + id : away; }
  function subject(f, follow) {
    var w = f.w, lane = (f.branches || [])[0] || '', o = (demo ? null : window.OPS) || { lanes: [] }, l = o.lanes.filter(function (x) { return x.branch === lane; })[0] || {};
    var room = f.goal ? f.goal.room : f.at ? f.at.room : '', links = [], slug = lane ? window.Board.slug(lane) : '';
    if (lane && !demo) links.push({ label: 'Its branch, ' + lane.replace(/^lane\/(show|sim)\//, ''), href: here(slug, 'floor.html#' + slug) });
    if (w.wait === 'owner') links.push({ label: 'What waits on you', href: here('queue', 'index.html#queue') });
    links.push({ label: 'The work in numbers', href: here('graphs', 'graphs.html') });
    var facts = [w.kind, f.leaving ? 'leaving' : room ? 'in the ' + H.ROOM[room].name.toLowerCase() : '', w.wait === 'owner' ? 'waiting on you' : w.state === 'working' ? 'at work' : w.kind === 'session' ? 'resting, ' + C.ago(w.age || 0) : 'asleep'];
    return { kind: 'worker', id: w.id, title: C.title(w), sub: C.doing(w) || w.what || '', lane: lane, assets: l.assets || [], links: links, facts: facts.filter(Boolean),
      kindLabel: w.kind === 'session' ? 'Claude session' : w.kind, worker: w, visual: w.visual || null, follow: !!follow };
  }
  // ---- who the panel is about. A drawer (house.html) is open on the frog the owner clicked, and shut otherwise.
  // Docked (the control screen) it is never empty: with no frog picked it follows one, `auto`: a session at work,
  // the one that moved last, and it stays with that one for as long as it works (two busy sessions do not swap
  // places on every reading); else whoever is at work; else the session that worked last.
  var docked = false, auto = null;
  function shown() { return pinned || (docked ? auto : null); }
  function choose() {
    var fs = Object.keys(sim.frogs).map(function (k) { return sim.frogs[k]; }).filter(function (f) { return !f.leaving; });
    if (auto && sim.frogs[auto.key] === auto && !auto.leaving && auto.w.state === 'working') return auto;
    function age(a, b) { return (a.w.age || 0) - (b.w.age || 0); }
    var at = fs.filter(function (f) { return f.w.state === 'working'; }), ses = function (f) { return f.w.kind === 'session'; };
    return at.filter(ses).sort(age)[0] || at[0] || fs.filter(ses).sort(age)[0] || null;
  }
  function tell() {
    var B = window.Board; if (!B) return;
    if (pinned && sim.frogs[pinned.key] !== pinned) pinned = null;          // it left the house
    if (pinned) { var f = pinned; B.open(subject(f), { onclose: function () { if (pinned !== f) return; pinned = null; dirty = true; showCard(hover); rows(); tell(); } }); return; }
    if (!docked) return;
    if (auto && sim.frogs[auto.key] === auto && B.busy && B.busy()) { B.open(subject(auto, true)); return; }      // the owner is writing about it
    var was = auto; auto = choose(); if (auto !== was) dirty = true;
    if (auto) B.open(subject(auto, true));
  }
  function pin(f) {
    pinned = f && pinned !== f ? f : null; dirty = true; showCard(pinned || hover); rows();
    if (!window.Board) return;
    if (pinned || docked) tell(); else window.Board.close();
    if (still) render();
    // on a narrow screen the profile is under the house: a click that changed something out of sight looked like nothing
    var pr = docked && pinned && document.getElementById('profile');
    if (pr && pr.getBoundingClientRect().top >= stage.getBoundingClientRect().bottom) pr.scrollIntoView({ block: 'nearest', behavior: still ? 'auto' : 'smooth' });
  }
  // by name, for the other parts of the page (a desk in the office): the frog with this key, id or uid
  function pick(key) {
    var k = Object.keys(sim.frogs).filter(function (k) { var w = sim.frogs[k].w; return k === key || w.uid === key || w.id === key; })[0];
    if (!k) return false;
    if (pinned !== sim.frogs[k]) pin(sim.frogs[k]);
    return true;
  }
  // the notice board in the hall, on the screen: a click on it goes to what waits on the owner
  function onBoard(px, py) {
    var th = H.THINGS.filter(function (t) { return t.kind === 'board'; })[0]; if (!th) return false;
    var b = th.box, a = toScreen.apply(null, iso(b[0], b[3], b[4])), c = toScreen.apply(null, iso(b[2], b[3], 0));
    return px >= Math.min(a[0], c[0]) && px <= Math.max(a[0], c[0]) && py >= Math.min(a[1], c[1]) - 6 && py <= Math.max(a[1], c[1]) + 6;
  }
  function go(room) { focus = focus === room ? '' : room; aim = frame(focus); chips(); dirty = true; if (still) { cam = { x: aim.x, y: aim.y, s: aim.s }; render(); } }
  function open(room) { if (focus !== room) go(room); }
  cv.addEventListener('mousemove', function (e) { var r = cv.getBoundingClientRect(), f = at(e.clientX - r.left, e.clientY - r.top); if (f !== hover) { hover = f; dirty = true; } cv.style.cursor = f || onBoard(e.clientX - r.left, e.clientY - r.top) ? 'pointer' : 'zoom-' + (focus ? 'out' : 'in'); cv.title = !f && onBoard(e.clientX - r.left, e.clientY - r.top) ? 'What waits on you' : ''; showCard(hover || pinned); });
  cv.addEventListener('mouseleave', function () { hover = null; dirty = true; showCard(pinned); });
  cv.addEventListener('click', function (e) { var r = cv.getBoundingClientRect(), f = at(e.clientX - r.left, e.clientY - r.top); if (f) return pin(f); if (onBoard(e.clientX - r.left, e.clientY - r.top)) { var qs = document.getElementById('queue'); if (qs) qs.scrollIntoView({ behavior: still ? 'auto' : 'smooth' }); else location.href = 'index.html#queue'; return; } var room = roomUnder(e.clientX - r.left, e.clientY - r.top); go(focus ? focus : room || ''); });
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape' && (focus || pinned) && !(window.Board && window.Board.busy && window.Board.busy())) { pinned = null; if (focus) go(focus); showCard(null); dirty = true; if (docked) tell(); } });

  // ---- the rooms as buttons, and everyone as a list (what the picture says, in words)
  function chips() {
    var nav = document.getElementById('h-rooms'); if (!nav) return;
    var n = sim.census(), sig = JSON.stringify([n, focus]);
    if (nav.dataset.sig === sig) return; nav.dataset.sig = sig;
    var had = nav.contains(document.activeElement) ? Array.prototype.indexOf.call(nav.children, document.activeElement) : -1;      // the buttons are made again: whoever was on one stays on it
    nav.innerHTML = '';
    var all = el('button', 'h-chip' + (focus ? '' : ' on'), 'Whole house'); all.type = 'button'; all.setAttribute('aria-pressed', focus ? 'false' : 'true'); all.addEventListener('click', function () { if (focus) go(focus); }); nav.appendChild(all);
    H.ROOMS.forEach(function (r) {
      var b = el('button', 'h-chip h-' + r.id + (focus === r.id ? ' on' : '') + (n[r.id].awake ? ' lit' : '')); b.type = 'button'; b.title = r.name + ': ' + r.does; b.setAttribute('aria-pressed', focus === r.id ? 'true' : 'false');
      b.appendChild(el('span', null, r.name)); b.appendChild(el('b', null, String(n[r.id].here))); b.addEventListener('click', function () { go(r.id); }); nav.appendChild(b);
    });
    if (had >= 0 && nav.children[had]) nav.children[had].focus();
  }
  function rows() {
    var box = document.getElementById('h-list'); if (!box) return;
    var sig = JSON.stringify([list.map(function (f) { return [f.key, f.goal && f.goal.room, f.w.state, f.w.doing, f.w.wait, f.leaving]; }), pinned && pinned.key]);
    if (box.dataset.sig === sig) return; box.dataset.sig = sig; box.innerHTML = '';
    H.ROOMS.filter(function (r) { return r.id !== 'bunk'; }).concat([H.ROOM.bunk]).forEach(function (r) {
      var here = list.filter(function (f) { return !f.leaving && f.goal && f.goal.room === r.id; }).sort(function (a, b) { return C.title(a.w).localeCompare(C.title(b.w)); });
      if (!here.length) return;
      var g = el('section', 'h-group h-' + r.id), hd = el('header'); hd.appendChild(el('b', null, r.name)); hd.appendChild(el('span', null, r.id === 'bunk' ? here.length + ' asleep' : r.does)); g.appendChild(hd);
      if (r.id === 'bunk') { var p = el('p', 'h-sleepers'); here.forEach(function (f) { var s = el('button', 'h-sleeper k-' + f.w.kind, C.title(f.w)); s.type = 'button'; s.title = f.w.what || ''; s.addEventListener('click', function () { if (focus !== 'bunk') go('bunk'); pin(f); }); p.appendChild(s); }); g.appendChild(p); }
      else here.forEach(function (f) {
        var row = el('button', 'h-row k-' + f.w.kind + (pinned === f ? ' on' : '')); row.type = 'button';
        row.appendChild(el('i')); row.appendChild(el('b', null, C.title(f.w)));
        row.appendChild(el('span', 'h-row-doing', f.w.wait === 'owner' ? 'waiting on you' : C.doing(f.w) || (f.w.kind === 'session' ? 'at work' : f.w.what) || ''));
        if (f.where) row.appendChild(el('span', 'h-row-where', f.where.replace(/^lane\/(show|sim)\//, '')));
        row.title = f.w.what || ''; row.addEventListener('click', function () { pin(f); }); g.appendChild(row);
      });
      box.appendChild(g);
    });
  }

  // ---- the readings, and time
  function words() {
    var st = document.getElementById('stamp'), Q = window.OwnerQueue; if (!st) return;
    if (demo) { st.textContent = 'a made-up floor, to look at the house (house.html shows the real one)'; return; }
    var f = Q ? Q.fresh(window.BEAT, Date.now()) : { stale: false };
    st.textContent = f.stale ? f.text + ': the watcher has stopped (ops.py --watch 20)' : 'read ' + String(window.BEAT || window.OPS_NOW || '').replace('T', ' ') + ', every 20 s';
    st.parentNode.classList.toggle('stale', !!f.stale);
  }
  var demoT = +q.get('t') || 0, demoRead = -1;
  function read() {
    var o = demo ? H.demo(demoT) : window.OPS;
    if (!o) return;
    sim.read(o); if (!demo) C.nowPill(o); words(); dirty = true;
    if (still) { for (var i = 0; i < 1200; i++) sim.step(0.1); render(); chips(); rows(); }
    tell();          // the panel is about someone on the floor: what it says follows the reading
  }
  function lights(dt) {
    var n = sim.census();
    H.ROOMS.forEach(function (r) { var want = n[r.id].awake || (n[r.id].coming && r.id !== 'bunk') ? 1 : r.id === 'bunk' ? 0.3 : r.id === 'hall' ? 0.86 : 0.5; lit[r.id] += (want - lit[r.id]) * Math.min(1, dt * 1.5); });
  }
  function advance(dt) {
    if (demo) { demoT += dt; var beat = Math.floor(demoT / 20); if (beat !== demoRead) { demoRead = beat; sim.read(H.demo(demoT)); } }
    sim.step(dt); lights(dt);
    var k = Math.min(1, dt * 5); cam.x += (aim.x - cam.x) * k; cam.y += (aim.y - cam.y) * k; cam.s += (aim.s - cam.s) * k;
  }
  var last = 0, owed = 0, seen = true;
  function tick(ts) {
    requestAnimationFrame(tick);
    var dt = Math.min(0.25, (ts - last) / 1000 || 0); last = ts;
    if (!seen || document.hidden) return;
    owed += dt; if (owed < 1 / 30) return;             // the loops are drawn at eight to twelve frames a second: thirty pictures a second is plenty
    advance(owed); owed = 0; render(); chips(); rows(); if (hover || pinned) showCard(hover || pinned);
  }
  if ('IntersectionObserver' in window) new IntersectionObserver(function (es) { seen = es[0].isIntersecting; }).observe(stage);
  if ('ResizeObserver' in window) new ResizeObserver(size).observe(stage); else window.addEventListener('resize', size);
  size();
  if (demo) { demoRead = Math.floor(demoT / 20); read(); } else C.live(read);
  var ff = +q.get('ff') || 0, pinKey = q.get('pin');
  for (var i = 0; i < ff * 10; i++) advance(0.1);
  cam = { x: aim.x, y: aim.y, s: aim.s }; lights(10);
  render(); chips(); rows(); ready = true;
  var prof = document.getElementById('profile');
  if (prof && window.Board && window.Board.dock) { docked = true; window.Board.dock(prof, function () { pinned = null; tell(); }); render(); }
  if (pinKey) { Object.keys(sim.frogs).some(function (k) { if (k.indexOf(pinKey) >= 0 || C.title(sim.frogs[k].w).indexOf(pinKey) >= 0) { pinned = sim.frogs[k]; return true; } }); render(); showCard(pinned); rows(); }
  // &bench: how long one picture takes here, in the page's title (sixty pictures, a thirtieth of a second apart)
  if (q.has('bench')) window.addEventListener('load', function () { render(); var t0 = performance.now(); for (var i = 0; i < 60; i++) { advance(1 / 30); render(); }
    document.getElementById('stamp').textContent = 'bench: ' + ((performance.now() - t0) / 60).toFixed(2) + ' ms a picture, ' + list.length + ' frogs, ' + cv.width + ' x ' + cv.height + ', sheets ' + Object.keys(sheets).filter(function (k) { return sheets[k].ok; }).length; });
  if (still || shot) { var poll = setInterval(function () { if (dirty) { dirty = false; render(); } }, 120); setTimeout(function () { clearInterval(poll); render(); document.documentElement.classList.add('h-ready'); }, 2500); }
  else requestAnimationFrame(tick);
  window.HouseView = { sim: sim, go: go, open: open, render: render, pin: pick, shown: shown };
})();
