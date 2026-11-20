"""The animation reference clips MiniMax H3 made for the frog-war units (2026-11-11, Documents/claude/unit-anim-refs):
per unit four actions (attack, move, hit, death), three takes each, 6 s at 2K, on the owner's Drive. The clips are big
(about 1 GB together), so they are not copied into the site: data/anims.js lists them with file:/// links the page plays
from the Drive, and only a small picture of each unit (img/anim/<unit>.jpg) is put in the site. The page is anims.html."""
import json
import os
from pathlib import Path

ROOT = Path(os.environ.get('TW_ANIMS', r'G:\My Drive\temp\aefadfsd'))
ACTIONS = ('attack', 'move', 'hit', 'death')
HERO = ('landscape.png', 'new.png')


def uri(p: Path) -> str:
    return p.resolve().as_uri()


def read(root: Path = ROOT):
    units = []
    if not root.is_dir():
        return units
    for d in sorted(root.iterdir(), key=lambda p: p.name.lower()):
        clips_dir = d / 'anim_h3'
        if not clips_dir.is_dir():
            continue
        prompts = {}
        pj = clips_dir / 'prompts.json'
        if pj.exists():
            try:
                prompts = {k: (v.get('prompt', '') if isinstance(v, dict) else str(v)) for k, v in json.loads(pj.read_text(encoding='utf-8')).items()}
            except Exception:       # noqa: BLE001  a broken prompt file is no reason to lose the clips
                prompts = {}
        clips = []
        for a in ACTIONS:
            for f in sorted(clips_dir.glob(f'{a}_*.mp4')):
                clips.append(dict(src=str(f), action=a, take=f.stem.split('_', 1)[1], url=uri(f), mb=round(f.stat().st_size / 1e6, 1), prompt=prompts.get(f.stem, '')))
        rejected = [dict(name=f.stem, url=uri(f)) for f in sorted((clips_dir / 'rejected').glob('*.mp4'))] if (clips_dir / 'rejected').is_dir() else []
        hero = next((d / h for h in HERO if (d / h).exists()), None)
        sheet = root / '_review_sheets' / f'{d.name}.jpg'
        units.append(dict(name=d.name, key=''.join(c if c.isalnum() else '-' for c in d.name.lower()), hero=hero, clips=clips, rejected=rejected,
                          sheet=uri(sheet) if sheet.exists() else '', folder=uri(clips_dir)))
    return units


def thumb(src: Path, dst: Path, w=480):
    if dst.exists() and dst.stat().st_mtime >= src.stat().st_mtime:
        return
    from PIL import Image
    dst.parent.mkdir(parents=True, exist_ok=True)
    im = Image.open(src).convert('RGB')
    im.thumbnail((w, w))
    im.save(dst, 'JPEG', quality=82)


def poster(src: Path, dst: Path, at=0.45, w=400):
    """A still of the clip (OpenCV, no ffmpeg here), so the page need not open 200 videos off the Drive to show them."""
    if dst.exists() and dst.stat().st_mtime >= src.stat().st_mtime:
        return True
    import cv2
    c = cv2.VideoCapture(str(src))
    n = c.get(cv2.CAP_PROP_FRAME_COUNT) or 0
    c.set(cv2.CAP_PROP_POS_FRAMES, int(n * at))
    ok, fr = c.read()
    c.release()
    if not ok:
        return False
    h = int(fr.shape[0] * w / fr.shape[1])
    dst.parent.mkdir(parents=True, exist_ok=True)
    ok, buf = cv2.imencode('.jpg', cv2.resize(fr, (w, h), interpolation=cv2.INTER_AREA), [cv2.IMWRITE_JPEG_QUALITY, 80])
    if ok:
        dst.write_bytes(buf.tobytes())
    return ok


def site(out: Path, root: Path = ROOT):
    units = read(root)
    rows = []
    for u in units:
        pic = ''
        if u['hero']:
            try:
                thumb(u['hero'], out / 'img' / 'anim' / f"{u['key']}.jpg")
                pic = f"img/anim/{u['key']}.jpg"
            except Exception:       # noqa: BLE001
                pic = ''
        for c in u['clips']:
            name = f"{u['key']}--{c['action']}_{c['take']}.jpg"
            try:
                c['poster'] = f'img/anim/{name}' if poster(Path(c['src']), out / 'img' / 'anim' / name) else ''
            except Exception:       # noqa: BLE001
                c['poster'] = ''
            c.pop('src', None)
        rows.append({k: v for k, v in u.items() if k != 'hero'} | dict(pic=pic))
    body = f'window.ANIMS = {json.dumps(dict(root=str(root), units=rows, actions=ACTIONS), sort_keys=True)};\n'
    f = out / 'data' / 'anims.js'
    f.parent.mkdir(parents=True, exist_ok=True)
    if not f.exists() or f.read_text(encoding='utf-8') != body:
        f.write_text(body, encoding='utf-8')
    return sum(len(u['clips']) for u in rows)


if __name__ == '__main__':
    import sys
    print(site(Path(sys.argv[1])), 'clips')
