#!/usr/bin/env python3
"""The page that plays a swept match back, from above, as moving dots.

    python Tools/sweepfilm.py <run folder or track.jsonl> [--out DIR]

A sweep run played with `sweep.py run <spec> --track` writes a track a match into <run>/tracks/<variant>-s<seed>.jsonl:
a header line with the trenches and their cell centres, then one line each five seconds with every man's place. This
turns each track into <run>/film/<stem>.html: one self-contained page, no network, the samples inlined as JSON, a
canvas from above, play/pause, a slider over the samples, the men alive per side in a readout, and under the slider a
thin two-colour strip of men alive over the whole match, so the minute it turned shows before any scrubbing. The arrow
keys step one sample. <run>/film/index.html links them all. Open it from the run folder; nothing is served.

A track with a header and no samples gets a page that says so. A track cut short in its last line loses that line
and keeps the rest; a track that will not read otherwise is named and skipped, and the others still get their pages.

Exit 0 and every path printed, 1 on a folder with no tracks, or when a track would not read at all.
Stdlib only, as sweep.py is.
"""
import argparse, json, sys
from pathlib import Path

sys.dont_write_bytecode = True

PAGE = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><title>__TITLE__</title>
<style>
 body { background: #15181c; color: #d7dbe0; font: 13px/1.5 system-ui, sans-serif; margin: 0; padding: 14px 16px; }
 h1 { font-size: 15px; font-weight: 600; margin: 0 0 2px; }
 .sub { color: #8d949c; margin-bottom: 10px; }
 .sub a { color: #8d949c; }
 canvas { background: #0e1013; border: 1px solid #2a2f36; display: block; }
 #field { width: 100%; height: auto; }
 #strip { width: 100%; height: 26px; margin-top: 6px; border-top: none; }
 .bar { display: flex; align-items: center; gap: 10px; margin-top: 8px; }
 button { background: #2a2f36; color: #d7dbe0; border: 1px solid #3a4048; border-radius: 3px;
          padding: 4px 12px; font: inherit; cursor: pointer; }
 button:hover { background: #343a43; }
 input[type=range] { flex: 1; }
 .read { font-variant-numeric: tabular-nums; white-space: nowrap; }
 .t0 { color: #6fb3ff; } .t1 { color: #ff8e6f; }
</style></head>
<body>
<h1>__TITLE__</h1>
<div class="sub">__SUB__ &mdash; <a href="index.html">every track of this run</a></div>
<canvas id="field" width="900" height="540"></canvas>
<canvas id="strip" width="900" height="26"></canvas>
<div class="bar">
  <button id="play">Play</button>
  <input type="range" id="slide" min="0" max="__LAST__" value="0" step="1">
  <span class="read" id="read"></span>
</div>
<script id="track" type="application/json">__DATA__</script>
<script>
const T = JSON.parse(document.getElementById('track').textContent);
const S = T.samples, head = T.head;
const field = document.getElementById('field'), g = field.getContext('2d');
const strip = document.getElementById('strip'), sg = strip.getContext('2d');
const slide = document.getElementById('slide'), read = document.getElementById('read');
const play = document.getElementById('play');
const COL = ['#6fb3ff', '#ff8e6f'], DIM = ['#2f5b8a', '#8a4e37'];

// The world's bounds over every trench cell and every man, so nothing moves out of the picture mid-play.
let lo = [1e9, 1e9], hi = [-1e9, -1e9];
function see(x, z) { if (x < lo[0]) lo[0] = x; if (x > hi[0]) hi[0] = x;
                     if (z < lo[1]) lo[1] = z; if (z > hi[1]) hi[1] = z; }
for (const tr of head.trenches) for (const c of tr.cells) see(c[0], c[1]);
for (const s of S) for (const m of s.men) see(m[0], m[1]);
if (hi[0] <= lo[0]) { lo[0] -= 1; hi[0] += 1; }
if (hi[1] <= lo[1]) { lo[1] -= 1; hi[1] += 1; }
const pad = 16, W = field.width, H = field.height;
const k = Math.min((W - 2 * pad) / (hi[0] - lo[0]), (H - 2 * pad) / (hi[1] - lo[1]));
const ox = (W - k * (hi[0] - lo[0])) / 2, oy = (H - k * (hi[1] - lo[1])) / 2;
// z grows away from seat 0, and the canvas grows down, so z is flipped: seat 0 at the bottom, as the camera sees it.
const px = x => ox + (x - lo[0]) * k, pz = z => H - oy - (z - lo[1]) * k;

const maxAlive = Math.max(1, ...S.map(s => Math.max(s.alive[0], s.alive[1])));

function drawField(i) {
  const s = S[i];
  g.clearRect(0, 0, W, H);
  head.trenches.forEach((tr, t) => {
    const owner = s.owners && s.owners.length > t ? s.owners[t] : tr.owner;
    g.strokeStyle = owner === 0 || owner === 1 ? DIM[owner] : '#454b54';
    g.lineWidth = 7; g.lineCap = 'round'; g.beginPath();
    tr.cells.forEach((c, j) => j ? g.lineTo(px(c[0]), pz(c[1])) : g.moveTo(px(c[0]), pz(c[1])));
    g.stroke();
  });
  for (const m of s.men) {
    if (m[4]) {   // died since the last sample: a dim ring where he fell, for this one sample
      g.strokeStyle = DIM[m[2]]; g.lineWidth = 1.5;
      g.beginPath(); g.arc(px(m[0]), pz(m[1]), 4.5, 0, 6.2832); g.stroke();
      continue;
    }
    g.fillStyle = COL[m[2]];
    g.beginPath(); g.arc(px(m[0]), pz(m[1]), m[3] ? 1.8 : 3.2, 0, 6.2832); g.fill();
  }
  read.innerHTML = fmt(s.t) + ' &nbsp; <span class="t0">' + s.alive[0] + '</span> v <span class="t1">'
                 + s.alive[1] + '</span> &nbsp; <span style="color:#8d949c">sample ' + (i + 1) + ' of ' + S.length + '</span>';
}

function fmt(t) { const m = Math.floor(t / 60); return m + ':' + String(Math.round(t - 60 * m)).padStart(2, '0'); }

// The strip: men alive over the whole match, seat 0 up from the bottom, seat 1 down from the top.
function drawStrip(i) {
  const w = strip.width, h = strip.height;
  sg.clearRect(0, 0, w, h);
  sg.fillStyle = '#0e1013'; sg.fillRect(0, 0, w, h);
  for (let j = 0; j < S.length; j++) {
    const x = S.length < 2 ? 0 : j * w / S.length, bw = Math.max(1, w / S.length);
    sg.fillStyle = COL[0]; sg.fillRect(x, h - S[j].alive[0] / maxAlive * (h / 2), bw, S[j].alive[0] / maxAlive * (h / 2));
    sg.fillStyle = COL[1]; sg.fillRect(x, 0, bw, S[j].alive[1] / maxAlive * (h / 2));
  }
  sg.fillStyle = '#ffffff';
  sg.fillRect(S.length < 2 ? 0 : i * w / S.length, 0, 1, h);
}

let at = 0, timer = null;
function show(i) { at = Math.max(0, Math.min(S.length - 1, i)); slide.value = at; drawField(at); drawStrip(at); }
function stop() { clearInterval(timer); timer = null; play.textContent = 'Play'; }
function start() {
  if (at >= S.length - 1) at = 0;
  play.textContent = 'Pause';
  timer = setInterval(() => { if (at >= S.length - 1) { stop(); return; } show(at + 1); }, 250);
}
play.onclick = () => timer ? stop() : start();
slide.oninput = () => { stop(); show(+slide.value); };
strip.onclick = e => { stop(); show(Math.floor(e.offsetX / strip.clientWidth * S.length)); };
document.addEventListener('keydown', e => {
  if (e.key === 'ArrowRight') { stop(); show(at + 1); }
  else if (e.key === 'ArrowLeft') { stop(); show(at - 1); }
  else if (e.key === ' ') { e.preventDefault(); timer ? stop() : start(); }
});
show(0);
</script>
</body></html>
"""

INDEX = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><title>__TITLE__</title>
<style>
 body { background: #15181c; color: #d7dbe0; font: 13px/1.5 system-ui, sans-serif; margin: 0; padding: 14px 16px; }
 h1 { font-size: 15px; font-weight: 600; margin: 0 0 10px; }
 table { border-collapse: collapse; }
 td, th { padding: 3px 14px 3px 0; text-align: left; border-bottom: 1px solid #2a2f36; }
 th { color: #8d949c; font-weight: 600; }
 a { color: #6fb3ff; }
</style></head>
<body><h1>__TITLE__</h1>
<table><tr><th>track</th><th>variant</th><th>seed</th><th>seating</th><th>length</th></tr>
__ROWS__
</table></body></html>
"""


EMPTY = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><title>__TITLE__</title>
<style>
 body { background: #15181c; color: #d7dbe0; font: 13px/1.5 system-ui, sans-serif; margin: 0; padding: 14px 16px; }
 h1 { font-size: 15px; font-weight: 600; margin: 0 0 2px; }
 .sub { color: #8d949c; margin-bottom: 10px; }
 .sub a { color: #8d949c; }
 p { color: #d7dbe0; }
</style></head>
<body>
<h1>__TITLE__</h1>
<div class="sub">__SUB__ &mdash; <a href="index.html">every track of this run</a></div>
<p>no samples: the match was over before the first five-second sample</p>
</body></html>
"""


class TrackError(Exception):
    """A track file whose header will not parse."""


def read_track(path):
    """One track as (head, samples). Raises TrackError naming the file."""
    try:
        lines = [ln for ln in Path(path).read_text(encoding='utf-8', errors='replace').splitlines() if ln.strip()]
    except OSError as e:
        raise TrackError(f'{path}: {e}')
    if not lines:
        raise TrackError(f'{path}: the track is empty')
    try:
        head = json.loads(lines[0])
    except ValueError as e:
        raise TrackError(f'{path}: the header is not JSON: {e}')
    if not isinstance(head, dict) or 'trenches' not in head:
        raise TrackError(f'{path}: the header has no trenches, so it is not a track header')
    samples = []
    for n, ln in enumerate(lines[1:], 2):
        try:
            samples.append(json.loads(ln))
        except ValueError as e:
            # The last line is the one a killed editor can leave half-written: drop it and keep the rest.
            if n == len(lines):
                print(f'sweepfilm: {path}: line {n} is cut short, dropped')
                return head, samples
            raise TrackError(f'{path}: line {n} is not JSON: {e}')
    return head, samples


def seating(head):
    return 'swapped' if head.get('swapped') else 'as seated'


def length(samples, head):
    if not samples:
        return '0:00'
    t = samples[-1].get('t', 0)
    return f'{int(t // 60)}:{round(t - 60 * (t // 60)):02d}'


def page(head, samples, stem):
    """One track's page: the samples inlined, nothing fetched."""
    title = f'{stem}'
    sub = (f'variant {head.get("variant", "?")}, seed {head.get("seed", "?")}, {seating(head)}, '
           f'{head.get("scenario", "?")} of {head.get("minutes", "?")} min, '
           f'{len(samples)} samples every {head.get("every", "?")} ticks')
    if not samples:
        return EMPTY.replace('__TITLE__', title).replace('__SUB__', sub)
    data = json.dumps({'head': head, 'samples': samples}, separators=(',', ':'))
    return (PAGE.replace('__TITLE__', title).replace('__SUB__', sub)
                .replace('__LAST__', str(max(0, len(samples) - 1))).replace('__DATA__', data))


def index(entries, label):
    rows = []
    for stem, head, samples in entries:
        rows.append(f'<tr><td><a href="{stem}.html">{stem}</a></td><td>{head.get("variant", "?")}</td>'
                    f'<td>{head.get("seed", "?")}</td><td>{seating(head)}</td><td>{length(samples, head)}</td></tr>')
    return INDEX.replace('__TITLE__', f'Sweep film {label}').replace('__ROWS__', '\n'.join(rows))


def write(path, body):
    path.write_text(body, encoding='utf-8', newline='\n')
    print(f'sweepfilm: {path}')


def main(argv=None):
    ap = argparse.ArgumentParser(description='Turn a sweep run\'s tracks into pages that play the match back as dots.')
    ap.add_argument('target', help='a sweep run folder, its tracks/ folder, or one track.jsonl')
    ap.add_argument('--out', help='where the pages go; <run>/film/ by default')
    args = ap.parse_args(argv)

    target = Path(args.target).resolve()
    if target.is_file():
        tracks, root = [target], target.parent.parent
    else:
        folder = target / 'tracks' if (target / 'tracks').is_dir() else target
        tracks = sorted(folder.glob('*.jsonl'))
        root = folder.parent if folder.name == 'tracks' else folder
        if not tracks:
            print(f'sweepfilm: {folder} holds no track (sweep.py run <spec> --track writes them)')
            return 1
    out = Path(args.out).resolve() if args.out else root / 'film'
    out.mkdir(parents=True, exist_ok=True)

    entries, bad = [], 0
    for t in tracks:
        try:
            head, samples = read_track(t)
        except TrackError as e:
            print(f'sweepfilm: {e}')
            bad += 1
            continue   # one unreadable track must not cost the pages of the others, nor the index
        write(out / (t.stem + '.html'), page(head, samples, t.stem))
        entries.append((t.stem, head, samples))
    write(out / 'index.html', index(entries, root.name))
    return 1 if bad or not entries else 0


if __name__ == '__main__':
    sys.exit(main())
