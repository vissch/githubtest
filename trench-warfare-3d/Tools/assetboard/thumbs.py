"""A preview picture for every model: one headless Blender run over the models that changed since the last build.

No Blender on this machine, or a job that fails: the asset keeps the preview already on the Drive (if any), and the
process page says so. Previews are keyed by what they were made from (the files' sizes and times, and this script),
so a rebuild renders only what changed.
"""
import hashlib
import json
import os
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent
BLENDER = Path(os.environ.get('TW_BLENDER', r'C:\Program Files\Blender Foundation\Blender 5.0\blender.exe'))
PLATE = (27, 29, 32)        # the board's plate colour, behind the transparent render
SHOW = 512


def stamp(paths):
    return [(str(p), Path(p).stat().st_size, int(Path(p).stat().st_mtime)) for p in paths if p and Path(p).exists()]


def jobs_for(P: Path, assets):
    jobs = []
    for a in assets.values():
        for m in a['models']:
            job = None
            if a['category'] == 'building' or a.get('chunk_rows'):
                rows = a.get('chunk_rows') or []
                set_dir = P / 'Resources/Env' / a['set']
                files, lo, hi = [], [1e9] * 3, [-1e9] * 3
                for r in rows:
                    f = next((c for c in (set_dir / f'{r["chunk"]}.fbx', set_dir / 'Chunks' / f'{r["chunk"]}.fbx') if c.exists()), None)
                    if f:
                        files.append(dict(fbx=str(f), at=r['offset']))
                        for k in range(3):
                            lo[k] = min(lo[k], r['offset'][k] + r['min'][k])
                            hi[k] = max(hi[k], r['offset'][k] + r['max'][k])
                expect = [hi[k] - lo[k] for k in range(3)] if files else None
                if m.get('whole'):     # a kit prop sliced in place keeps its own pivot: its whole FBX is the true picture
                    files, expect = [dict(fbx=str(P / m['whole']), at=None)], None
                if files:
                    job = dict(files=files, texture=str(P / m['texture']) if m.get('texture') else None, fix=False, hide=[], expect=expect)
            elif m['form'] in ('battle', 'trial') and a['category'] == 'vehicle' and m['lods']:
                job = dict(files=[dict(fbx=str(P / m['lods'][0]['path']), at=None)], texture=str(P / m['texture']) if m.get('texture') else None,
                           fix=True, hide=[], expect=None)
            elif a['category'] == 'character' and m['form'] == 'trial' and m['lods']:
                job = dict(files=[dict(fbx=str(P / m['lods'][0]['path']), at=None)], texture=str(P / m['texture']) if m.get('texture') else None,
                           fix=False, hide=['LOD1', 'LOD2', 'LOD3'], expect=None)
            elif a['category'] == 'character' and m['form'] == 'battle' and m.get('source'):
                job = dict(files=[dict(fbx=str(P / m['source']), at=None)], texture=None, fix=False, hide=[], expect=None,
                           view=[0.75, -1.0, 0.45])     # the Mixamo figures face Blender's -Y
            if job:
                job['id'] = f'{a["id"]}.{m["form"]}'
                job['asset'], job['model'] = a, m
                jobs.append(job)
    return jobs


def attach(P: Path, assets, stage: Path, out: Path, local: Path, meta):
    from PIL import Image
    jobs = jobs_for(P, assets)
    cache = local / 'thumbs'
    cache.mkdir(parents=True, exist_ok=True)
    (stage / 'thumb').mkdir(exist_ok=True)
    index_file = out / 'data' / 'thumbs.json'
    index = json.loads(index_file.read_text(encoding='utf-8')) if index_file.exists() else {}
    script = (HERE / 'thumb_blender.py').read_bytes()
    todo = []
    for j in jobs:
        srcs = [f['fbx'] for f in j['files']] + [j['texture']]
        j['key'] = hashlib.sha1(script + json.dumps([stamp(srcs), [f['at'] for f in j['files']], j['fix'], j['hide'], j.get('view')]).encode()).hexdigest()[:16]
        j['file'] = f'thumb/{j["id"]}.jpg'
        j['png'] = str(cache / f'{j["id"]}.png')
        have = index.get(j['id'], {})
        if have.get('key') == j['key'] and (out / j['file']).exists():
            j['result'] = have
        else:
            todo.append(j)
    meta['sources']['blender'] = dict(found=BLENDER.exists(), rendered=0, reused=len(jobs) - len(todo), failed=[])
    if todo and BLENDER.exists():
        spec = cache / 'jobs.json'
        res = cache / 'results.json'
        res.unlink(missing_ok=True)
        spec.write_text(json.dumps([dict(id=j['id'], out=j['png'], files=j['files'], texture=j['texture'], fix=j['fix'], hide=j['hide'],
                                         expect=j['expect'], view=j.get('view')) for j in todo]), encoding='utf-8')
        subprocess.run([str(BLENDER), '-b', '--factory-startup', '-P', str(HERE / 'thumb_blender.py'), '--', str(spec), str(res)],
                       capture_output=True, timeout=3600)
        done = json.loads(res.read_text(encoding='utf-8')) if res.exists() else {}
        for j in todo:
            r = done.get(j['id'], dict(ok=False, error='Blender wrote no result for it'))
            if r.get('ok') and Path(j['png']).exists():
                img = Image.open(j['png']).convert('RGBA')
                plate = Image.new('RGBA', img.size, PLATE + (255,))
                plate.alpha_composite(img)
                plate.convert('RGB').resize((SHOW, SHOW), Image.LANCZOS).save(stage / j['file'], quality=86)
                j['result'] = dict(key=j['key'], ok=True, size=r.get('size'), warn=r.get('warn'), built=meta['built'])
                meta['sources']['blender']['rendered'] += 1
            else:
                j['result'] = dict(key=None, ok=False, error=r.get('error', 'no picture written'))
                meta['sources']['blender']['failed'].append(f'{j["id"]}: {j["result"]["error"]}')
    elif todo:
        meta['warnings'].append(f'thumbnails: no Blender at {BLENDER}; {len(todo)} previews not rendered (set TW_BLENDER)')
        for j in todo:
            j['result'] = dict(key=None, ok=False, error='no Blender on this machine')
    for j in jobs:
        r = j['result']
        index[j['id']] = r if r.get('ok') else index.get(j['id'], r)
        kept = index[j['id']]
        if kept.get('ok') and ((stage / j['file']).exists() or (out / j['file']).exists()):
            j['model']['thumb'] = j['file']
            j['model']['thumb_warn'] = kept.get('warn')
            j['model']['thumb_stale'] = kept.get('key') != j['key']
            if kept.get('size') and j['asset']['category'] != 'building':
                j['asset']['measurements'].setdefault('Size (m)', ' x '.join(f'{v:.1f}' for v in kept['size']))
            if kept.get('size') and j['asset']['category'] == 'building':
                j['asset']['measurements']['Size (m)'] = ' x '.join(f'{v:.1f}' for v in kept['size'])
        else:
            j['model']['thumb_error'] = r.get('error')
    (stage / 'data').mkdir(exist_ok=True)
    (stage / 'data' / 'thumbs.json').write_text(json.dumps(index, indent=1), encoding='utf-8')
    for f in meta['sources']['blender']['failed']:
        meta['warnings'].append('thumbnail failed: ' + f)
