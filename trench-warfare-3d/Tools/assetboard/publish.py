"""Copy a built site into its folder, writing only the files whose bytes changed.

The folder is on a synced Drive: every rewrite is an upload, on both stations. Pictures and films (img/, thumb/, film/, look/) are never
deleted here, because the other station may have harvested ones this machine cannot see; pages and data are replaced.
The site has a budget, so a mistake in a harvest cannot fill the Drive.
"""
import shutil
from pathlib import Path

MAX_FILES = 2500
MAX_BYTES = 400 * 1024 * 1024        # the films are most of it


def sync(stage: Path, out: Path):
    out.mkdir(parents=True, exist_ok=True)
    written = kept = 0
    for src in sorted(p for p in stage.rglob('*') if p.is_file()):
        dst = out / src.relative_to(stage)
        if dst.exists() and dst.stat().st_size == src.stat().st_size and dst.read_bytes() == src.read_bytes():
            kept += 1
            continue
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dst)
        written += 1
    pages = {p.relative_to(stage).as_posix() for p in (stage / 'a').glob('*.html')} if (stage / 'a').is_dir() else set()
    if pages:                     # a page for an asset that no longer exists is removed; pictures stay
        for old in (out / 'a').glob('*.html'):
            if old.relative_to(out).as_posix() not in pages:
                old.unlink()
    files = [p for p in out.rglob('*') if p.is_file()]
    size = sum(p.stat().st_size for p in files)
    if len(files) > MAX_FILES or size > MAX_BYTES:
        print(f'assetboard: the site is {len(files)} files, {size // (1024 * 1024)} MB: over its budget of {MAX_FILES} files, '
              f'{MAX_BYTES // (1024 * 1024)} MB. Prune img/ by hand, or lower PER_ASSET in src_images.py.')
    shutil.rmtree(stage, ignore_errors=True)
    return written, kept
