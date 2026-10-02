#!/usr/bin/env python3
"""The asset board: a static site over the art assets and their process. Characters, vehicles, buildings.

WHY. Nothing said, in one place, which units have a model of their own, which are drawn with another unit's, what
is being worked on and on which lane, or what a building looks like. Every answer was in the code tables, the
manifests, the branches and the docs, so this derives the board from them and never stores a status of its own.

    python Tools/assetboard/build.py            from trench-warfare-3d/: build the site
    python Tools/assetboard/build.py --check    read the code and the files, print the counts, write nothing
    python Tools/assetboard/build.py --json-only --out DIR    assets.json only
    python Tools/assetboard/build.py --no-thumbs / --no-images / --no-git    skip a slow part

The site goes to TW_ASSETBOARD_OUT, else to the Drive folder TW3D-pipeline/assets when that Drive is mounted, else to
a folder under %LOCALAPPDATA%/TrenchWarfare/assetboard. Nothing it writes belongs in the repo.

What only a person knows (priority, notes, planned assets, which unit a shared figure was made for) is in
docs/reference/asset-notes.json. The rules for each status are at the top of model.py.

A source that is missing on this machine (no Blender, no gym runs, no board repo) is a warning on the process page,
not a failure. A code table that changed shape IS a failure: src_code.py names the file and what it expected.
"""
import argparse
import datetime
import json
import os
import socket
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]                       # trench-warfare-3d
REPO = ROOT.parent
P = ROOT / 'Assets' / '_Project'
sys.path.insert(0, str(HERE))
import model      # noqa: E402
import src_code   # noqa: E402

NOTES = REPO / 'docs' / 'reference' / 'asset-notes.json'
DRIVE = Path('G:/My Drive/TW3D-pipeline')
LOCAL = Path(os.environ.get('LOCALAPPDATA', tempfile.gettempdir())) / 'TrenchWarfare' / 'assetboard'


def out_dir(arg):
    if arg:
        return Path(arg)
    if os.environ.get('TW_ASSETBOARD_OUT'):
        return Path(os.environ['TW_ASSETBOARD_OUT'])
    return DRIVE / 'assets' if DRIVE.is_dir() else LOCAL / 'site'


def collect(args, warnings):
    code = src_code.read_all(P)
    notes = model.load_notes(NOTES)
    assets, extra = model.build(P, code, notes)
    lanes = []
    if not args.no_git:
        import src_git
        lanes = src_git.attach(REPO, assets, warnings)
        import src_docs
        src_docs.attach(REPO, assets, warnings)
        import src_board
        extra['board'] = src_board.attach(ROOT, assets, warnings)
    for a in assets.values():
        model.decide(a)
    return code, assets, extra, lanes


def counts(assets):
    out = {}
    for a in assets.values():
        out.setdefault(a['category'], {}).setdefault(a['status'], []).append(a['id'])
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='build the asset board')
    ap.add_argument('--check', action='store_true')
    ap.add_argument('--json-only', action='store_true')
    ap.add_argument('--no-thumbs', action='store_true')
    ap.add_argument('--no-images', action='store_true')
    ap.add_argument('--no-films', action='store_true')
    ap.add_argument('--no-git', action='store_true')
    ap.add_argument('--out', default='')
    args = ap.parse_args(argv)
    warnings = []
    try:
        code, assets, extra, lanes = collect(args, warnings)
    except (src_code.ProbeError, model.NotesError) as e:
        print(f'assetboard: {e}')
        return 1
    by = counts(assets)
    for cat in ('character', 'vehicle', 'building'):
        total = sum(len(v) for v in by.get(cat, {}).values())
        print(f'{cat:10} {total:3}  ' + '  '.join(f'{model.STATUS_LABEL[s]}: {len(by.get(cat, {}).get(s, []))}' for s in model.STATUSES))
    if args.check:
        for w in warnings:
            print('warning: ' + w)
        return 0

    out = out_dir(args.out)
    stage = Path(tempfile.mkdtemp(prefix='tw-assetboard-'))
    meta = dict(schema=1, built=datetime.datetime.now().strftime('%Y-%m-%d %H:%M'), station=socket.gethostname(),
                out=str(out), warnings=warnings, sources={})
    import src_git
    meta['commit'], meta['branch'], meta['refs_as_of'] = src_git.head(REPO)
    if not args.json_only:
        if not args.no_images:
            import src_images
            src_images.attach(REPO, P, assets, stage, out, meta)
        if not args.no_thumbs:
            import thumbs
            thumbs.attach(P, assets, stage, out, LOCAL, meta)
        if not args.no_thumbs:                       # --no-films keeps the films already made and renders none
            import films
            films.attach(P, assets, code, stage, out, LOCAL, meta, only_harvest=args.no_films)
            if not args.no_git:
                import looks
                looks.attach(REPO, P, assets, stage, out, LOCAL, meta)
    (stage / 'data').mkdir(exist_ok=True)
    slim = [{k: v for k, v in a.items() if k not in ('chunk_rows',)} for a in assets.values()]
    (stage / 'data' / 'assets.json').write_text(json.dumps(dict(meta=meta, assets=slim), indent=1, default=str), encoding='utf-8')
    if not args.json_only:
        import render
        render.site(assets, code, extra, lanes, meta, stage)
    import publish
    written, kept = publish.sync(stage, out)
    if not args.json_only and not args.no_git:     # the floor page and a first reading of it (ops.py --watch keeps it live)
        import ops
        ops.page(out, meta)
        ops.once(out)
    print(f'assetboard: {written} files written, {kept} unchanged, in {out}')
    for w in warnings:
        print('warning: ' + w)
    return 0


if __name__ == '__main__':
    sys.exit(main())
