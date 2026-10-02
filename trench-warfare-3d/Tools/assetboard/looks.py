"""How a model looked at each commit that changed it: one picture per version, oldest first.

The versions come from git (the commits on this branch that touched the model's first LOD or its texture; for a
battle figure, its baked mesh or atlas). Each version's files are read out of git into a cache and drawn by the same
headless Blender scripts as the previews, so the pictures compare like with like. A picture is named by the blobs it
was made from, so it is drawn once and never again. Only models with more than one version get a strip.

A building's chunks are many files and its versions are not followed here.
"""
import hashlib
import json
import subprocess
from pathlib import Path

import thumbs

HERE = Path(__file__).resolve().parent
MOST = 8          # versions shown per model, the latest ones


def git(repo, *args, binary=False):
    r = subprocess.run(['git', '-C', str(repo), *args], capture_output=True)
    if r.returncode != 0:
        return None
    return r.stdout if binary else r.stdout.decode('utf-8', 'replace')


def versions(repo: Path, paths):
    """[(sha, date, subject, [blob per path])] oldest first, one entry per distinct set of blobs."""
    rels = [Path(p).resolve().relative_to(repo.resolve()).as_posix() for p in paths]
    log = git(repo, 'log', '--format=%H\t%ad\t%s', '--date=short', '--', *rels) or ''
    out = []
    for line in reversed(log.strip().split('\n')):
        if not line.strip():
            continue
        sha, date, subject = line.split('\t', 2)
        blobs = [(git(repo, 'rev-parse', '--verify', '-q', f'{sha}:{rel}') or '').strip() for rel in rels]
        if not blobs[0] or (out and out[-1][3] == blobs):
            continue
        out.append((sha, date, subject, blobs))
    return out[-MOST:]


def blob_file(repo: Path, cache: Path, blob, suffix):
    f = cache / f'{blob}{suffix}'
    if not f.exists():
        data = git(repo, 'cat-file', 'blob', blob, binary=True)
        if data is None:
            return None
        f.write_bytes(data)
    return f


def attach(repo: Path, P: Path, assets, stage: Path, out: Path, local: Path, meta):
    from PIL import Image
    cache = local / 'looks'
    cache.mkdir(parents=True, exist_ok=True)
    (stage / 'look').mkdir(exist_ok=True)
    thumb_jobs, figure_jobs, wanted, seen = [], [], [], {}
    for a in assets.values():
        for m in a['models']:
            figure = a.get('figure') if a['category'] == 'character' and m['form'] == 'battle' else None
            if figure:
                paths = [P / 'Resources/Units' / f'Figure{figure}Mesh.asset', P / 'Resources/Units' / f'Figure{figure}Atlas.bytes']
                stem = f'Figure{figure}'
            elif a['category'] != 'building' and m.get('lods') and not m.get('chunks') and m['form'] in ('battle', 'trial'):
                paths = [P / m['lods'][0]['path']] + ([P / m['texture']] if m.get('texture') else [])
                stem = f'{a["id"]}.{m["form"]}'
            else:
                continue
            if not all(p.exists() for p in paths):
                continue
            if stem not in seen:
                seen[stem] = versions(repo, paths)
            vs = seen[stem]
            if len(vs) < 2:
                continue
            looks = []
            for sha, date, subject, blobs in vs:
                name = f'{stem}.{hashlib.sha1("".join(blobs).encode()).hexdigest()[:10]}'
                file = f'look/{name}.jpg'
                looks.append(dict(sha=sha[:8], date=date, subject=subject, file=file))
                if (out / file).exists() or any(j['id'] == name for j in thumb_jobs + figure_jobs):
                    continue
                files = [blob_file(repo, cache, b, p.suffix) if b else None for b, p in zip(blobs, paths)]
                if files[0] is None:
                    continue
                if figure:
                    if files[1] is None:
                        continue
                    figure_jobs.append(dict(id=name, kind='figure', out=(cache / name).as_posix(), fps=24, size=640, turn=1 / 24,
                                            mesh=str(files[0]), atlas=str(files[1]), rows=[dict(row=1, name='Idle')]))
                else:
                    thumb_jobs.append(dict(id=name, out=str(cache / f'{name}.png'), files=[dict(fbx=str(files[0]), at=None)],
                                           texture=str(files[1]) if len(files) > 1 and files[1] else None, fix=a['category'] == 'vehicle',
                                           hide=['LOD1', 'LOD2', 'LOD3'] if a['category'] == 'character' else [], expect=None, view=None))
            wanted.append((m, looks))
    report = meta['sources']['looks'] = dict(models=len(wanted), drawn=0, failed=[])
    if (thumb_jobs or figure_jobs) and thumbs.BLENDER.exists():
        for script, jobs in (('thumb_blender.py', thumb_jobs), ('film_blender.py', figure_jobs)):
            if not jobs:
                continue
            spec, res = cache / 'jobs.json', cache / 'results.json'
            res.unlink(missing_ok=True)
            spec.write_text(json.dumps(jobs), encoding='utf-8')
            subprocess.run([str(thumbs.BLENDER), '-b', '--factory-startup', '-P', str(HERE / script), '--', str(spec), str(res)], capture_output=True, timeout=3600)
            done = json.loads(res.read_text(encoding='utf-8')) if res.exists() else {}
            for j in jobs:
                png = Path(j['out']) / '00000.png' if j.get('kind') == 'figure' else Path(j['out'])
                if done.get(j['id'], {}).get('ok') and png.exists():
                    img = Image.open(png).convert('RGBA')
                    plate = Image.new('RGBA', img.size, thumbs.PLATE + (255,))
                    plate.alpha_composite(img)
                    plate.convert('RGB').resize((thumbs.SHOW, thumbs.SHOW), Image.LANCZOS).save(stage / 'look' / f'{j["id"]}.jpg', quality=86)
                    report['drawn'] += 1
                else:
                    report['failed'].append(f'{j["id"]}: {done.get(j["id"], {}).get("error", "no picture")}')
    for m, looks in wanted:
        m['looks'] = [l for l in looks if (stage / l['file']).exists() or (out / l['file']).exists()]
    for f in report['failed']:
        meta['warnings'].append('an earlier version could not be drawn: ' + f)
