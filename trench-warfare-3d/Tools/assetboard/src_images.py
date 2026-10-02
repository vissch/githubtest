"""The pictures of an asset that already exist, copied (downscaled) into the site so it stands alone on the Drive.

In the repo: card portraits, cut-outs, mood busts, texture atlases, the figures under docs/reference. On this machine
only: the gym's unit strips (a still per unit, per run) and the board's evidence images. On the Drive: films, which
are linked and never copied. A source that is not on this machine is skipped and named in meta['sources']; what an
earlier build (here or on the other station) put in the site's img/ folder stays, because publish never prunes it.

A picture's file name carries a hash of its source bytes, so an unchanged picture is never rewritten.
"""
import hashlib
import os
import re
import subprocess
from pathlib import Path

GYM = Path(os.environ.get('LOCALAPPDATA', '')) / 'TrenchWarfare' / 'gym'
GYM_RUNS = 3                 # the last few runs: a unit's look over the last days
PER_ASSET = 12
IMG = ('.png', '.jpg', '.jpeg')


def put(src: Path, stage: Path, out: Path, aid: str, kind: str, width: int):
    """Copy one picture into img/<asset>/ at most `width` wide. Returns its path in the site, or None."""
    from PIL import Image
    try:
        data = src.read_bytes()
    except OSError:
        return None
    digest = hashlib.sha1(data).hexdigest()[:8]
    keep_png = src.suffix.lower() == '.png' and len(data) < 400_000
    relp = f'img/{aid}/{kind}-{digest}.{"png" if keep_png else "jpg"}'
    if (out / relp).exists() or (stage / relp).exists():
        return relp
    (stage / relp).parent.mkdir(parents=True, exist_ok=True)
    if keep_png:
        (stage / relp).write_bytes(data)
        return relp
    try:
        im = Image.open(src)
        if im.mode in ('RGBA', 'LA', 'P'):
            im = im.convert('RGBA')
            plate = Image.new('RGBA', im.size, (27, 29, 32, 255))
            plate.alpha_composite(im)
            im = plate
        im = im.convert('RGB')
        if im.width > width:
            im = im.resize((width, round(im.height * width / im.width)), Image.LANCZOS)
        im.save(stage / relp, quality=84)
    except Exception:
        return None
    return relp


def attach(repo: Path, P: Path, assets, stage: Path, out: Path, meta):
    def add(a, src, kind, date, caption, width=1280):
        if sum(1 for i in a['images'] if i['kind'] not in ('portrait', 'unitart', 'mood', 'atlas')) >= PER_ASSET and kind not in ('portrait', 'unitart', 'mood', 'atlas'):
            return
        f = put(Path(src), stage, out, a['id'], kind, width)
        if f and not any(i['file'] == f for i in a['images']):
            a['images'].append(dict(file=f, kind=kind, date=date, caption=caption))

    by_alias = {}
    for a in assets.values():
        for al in a['aliases']:
            by_alias.setdefault(al.lower(), []).append(a)
        if a.get('figure') and not a['drawn_as'] and a['category'] == 'character':
            by_alias.setdefault(a['figure'].lower(), []).append(a)        # "soldier" means the units made for that figure

    # ---- in the repo
    for a in assets.values():
        pics = a.get('pictures') or {}
        if pics.get('unitart'):
            add(a, P / pics['unitart'], 'unitart', None, 'UI cut-out (UnitArt)', 512)
        if pics.get('portrait'):
            add(a, P / pics['portrait'], 'portrait', None, 'Card portrait' + (' (generated placeholder)' if pics.get('placeholder') else ''), 256)
        for m in pics.get('moods', []):
            add(a, P / m, 'mood', None, 'Mood bust: ' + Path(m).stem.rsplit('_', 1)[-1], 256)
        for m in a['models']:
            if m.get('texture') and a['category'] != 'building':
                add(a, P / m['texture'], 'atlas', None, f'Texture of the {m["form"]} model (its UV layout, not a picture of it)', 512)
    figures = repo / 'docs/reference/figures'
    dated = {}
    if figures.is_dir():
        log = subprocess.run(['git', '-C', str(repo), 'log', '--date=short', '--format=@@%ad', '--name-only', '--diff-filter=A', '--', 'docs/reference/figures'],
                             capture_output=True).stdout.decode('utf-8', 'replace')
        date = None
        for line in log.split('\n'):
            if line.startswith('@@'):
                date = line[2:]
            elif line.strip():
                dated[line.strip()] = date
        for f in sorted(figures.rglob('*')):
            if f.suffix.lower() in IMG:
                tokens = set(re.split(r'[-_.\s]+', f.stem.lower()))
                for al, group in by_alias.items():
                    if al in tokens:
                        for a in group:
                            add(a, f, 'doc', dated.get(f.relative_to(repo).as_posix()), 'docs/reference/figures: ' + f.stem)

    # ---- on this machine: the gym's unit strips
    runs = sorted(d for d in GYM.iterdir() if (d / 'Units').is_dir()) if GYM.is_dir() else []
    meta['sources']['gym'] = dict(found=bool(runs), latest=runs[-1].name if runs else None, runs=len(runs))
    for run in reversed(runs[-GYM_RUNS:]):
        m = re.match(r'(\d{4})(\d{2})(\d{2})-(\d{4})-(\w+)', run.name)
        date = f'{m.group(1)}-{m.group(2)}-{m.group(3)}' if m else None
        sha = m.group(5) if m else ''
        for f in sorted((run / 'Units').glob('*.jpg')):
            for a in by_alias.get(f.stem.lower(), []):
                if a['kind'] == 'unit':
                    add(a, f, 'gym', date, f'In the game: gym strip, run {run.name[:13]} at {sha}', 1600)

    # ---- on this machine: the board's evidence for the items that are about an asset
    board = repo.parent / 'tw3d-board' / 'evidence'
    meta['sources']['board_evidence'] = dict(found=board.is_dir())
    if board.is_dir():
        for a in assets.values():
            for item in sorted({b['item'] for b in a['board']}):
                files = sorted(f for f in (board / item).rglob('*') if f.suffix.lower() in IMG)
                named = [f for f in files if set(re.split(r'[-_.\s]+', f.stem.lower())) & {al.lower() for al in a['aliases']}]
                for f in (named or files)[:4]:
                    add(a, f, 'evidence', None, f'Board evidence, {item}: {f.relative_to(board / item).as_posix()}')

    # ---- on the Drive: films, linked
    films = Path('G:/My Drive/TW3D-pipeline/films')
    meta['sources']['films'] = dict(found=films.is_dir())
    if films.is_dir():
        on_drive = out.parent == films.parent
        for f in sorted(films.rglob('*.mp4')):
            head = re.split(r'[-_.\s]+', f.stem.lower())[0]
            for a in by_alias.get(head, []):
                date = re.match(r'\d{4}-\d{2}-\d{2}', f.parent.name)
                link = ('../../films/' + f.relative_to(films).as_posix()) if on_drive else f.as_uri()
                a['films'].append(dict(link=link, name=f'{f.parent.name}/{f.name}', date=date.group(0) if date else None))
    for a in assets.values():
        a['images'].sort(key=lambda i: (i['date'] or '', i['kind']), reverse=True)
