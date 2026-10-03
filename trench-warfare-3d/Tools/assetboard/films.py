"""Films of every model: once round on a turntable, the battle figures playing every clip they are baked with, the
buildings drawn apart into their chunks.

Headless Blender renders the frames (film_blender.py), this file writes the clip names on them and ffmpeg makes the
mp4. A film is keyed by what it was made from, so a rebuild renders only what changed; a first build of everything is
about 25,000 frames and takes some minutes. No Blender or no ffmpeg on this machine: the films already on the Drive
are kept and the process page says so.

These are renders of the model and of the baked animation data, not captures of the game: no effects, no terrain,
no shader of the game's. Films of the game itself are harvested from Captures/assetfilm (gamefilm.py makes them).
"""
import gzip
import hashlib
import json
import os
import re
import shutil
import struct
import subprocess
import time
from pathlib import Path

import thumbs

HERE = Path(__file__).resolve().parent
FFMPEG = os.environ.get('TW_FFMPEG') or shutil.which('ffmpeg')
FFPROBE = shutil.which('ffprobe')
SIZE, FPS = 640, 24
TURN_SECONDS, APART_SECONDS = 6.0, 4.0
WORKERS = 4
BONE, DIM = (232, 226, 210), (150, 148, 140)
GROUP_TITLE = {'idle': 'Idle', 'locomotion': 'Movement', 'fire': 'Shooting', 'actions': 'Reload, grenade, melee', 'reactions': 'Hit and dodge',
               'trench and stance': 'Trench and stance', 'deaths': 'Deaths'}


def words(name):
    """FireStand -> Fire stand; Turn90L -> Turn 90 L."""
    s = re.sub(r'(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Za-z])(?=[0-9])', ' ', name)
    return s[0] + s[1:].lower() if not s.isupper() else s


def slug(s):
    return re.sub(r'[^a-z0-9]+', '-', s.lower()).strip('-')


def atlas_rows(path):
    """How many rows (clips) a baked atlas holds: the third number of its header (VatCodec.Encode)."""
    with gzip.open(path) as f:
        head = f.read(64)
    return struct.unpack_from('<iii', head, 1 + head[0])[2]


def jobs_for(P: Path, assets, code, warnings=None):
    """One list of films over the distinct models: three units drawn with one figure share its films."""
    jobs, seen = [], {}

    def add(job, a, m, title, order):
        have = seen.get(job['id'])
        if have is None:
            job['title'], job['order'], job['models'] = title, order, []
            seen[job['id']] = job
            jobs.append(job)
            have = job
        have['models'].append((a, m))

    for t in thumbs.jobs_for(P, assets):
        a, m = t['asset'], t['model']
        base = dict(fps=FPS, size=SIZE, files=t['files'], texture=t['texture'], fix=t['fix'], hide=t['hide'], view=t.get('view'))
        figure = a.get('figure') if a['category'] == 'character' and m['form'] == 'battle' else None
        mesh = P / 'Resources/Units' / f'Figure{figure}Mesh.asset' if figure else None
        atlas = P / 'Resources/Units' / f'Figure{figure}Atlas.bytes' if figure else None
        if figure and mesh.exists() and atlas.exists():
            fig = dict(kind='figure', fps=FPS, size=SIZE, mesh=str(mesh), atlas=str(atlas), sources=[str(mesh), str(atlas)])
            add(dict(fig, id=f'Figure{figure}.turn', turn=TURN_SECONDS, rows=[dict(row=1, name='Idle')]), a, m, 'Turntable', 0)
            groups = {}
            if atlas_rows(atlas) != len(code['clips']):      # a row is a clip by its place in the enum: a stale bake would be captioned wrongly
                if warnings is not None and not any(figure in w for w in warnings):
                    warnings.append(f'films: Figure{figure}Atlas has {atlas_rows(atlas)} rows and the Clip enum {len(code["clips"])}: '
                                    f'bake it again (TW/VAT/Bake Infantry); only its turntable is filmed')
                continue
            for name, row, group in code.get('clips', []):
                if row > 0:
                    groups.setdefault(group, []).append(dict(row=row, name=words(name)))
            for k, (group, rows) in enumerate(groups.items()):
                add(dict(fig, id=f'Figure{figure}.{slug(group)}', rows=rows), a, m, GROUP_TITLE.get(group, group.capitalize()), 1 + k)
            continue
        srcs = [f['fbx'] for f in t['files']] + [t['texture']]
        stem = a['id'] if a['category'] == 'building' else f'{a["id"]}.{m["form"]}'
        add(dict(base, id=f'{stem}.turn', kind='turn', seconds=TURN_SECONDS, sources=srcs), a, m, 'Turntable', 0)
        if a['category'] == 'building' and len(t['files']) > 1:
            add(dict(base, id=f'{stem}.apart', kind='apart', seconds=APART_SECONDS, sources=srcs), a, m, 'Its chunks, drawn apart', 1)
    return jobs


GAME_TITLE = {'turn': 'In the game: turntable', 'moves': 'In the game: moving and firing', 'destroyed': 'In the game: shot to pieces',
              'burning': 'In the game: set alight', 'shelled': 'In the game: shelled', 'battle': 'In a battle'}
GAME_ORDER = list(GAME_TITLE)


def harvest(root: Path, assets, stage: Path, out: Path, index, report):
    """The game's own films (gamefilm.py leaves them in Captures/assetfilm as <asset>[.<form>].game-<what>.mp4): copied
    in when this station has them, kept from the Drive when it does not."""
    src = root / 'Captures' / 'assetfilm'
    game = index.setdefault('game', {})
    if src.is_dir():
        for mp4 in sorted(src.glob('*.game-*.mp4')):
            poster = mp4.with_suffix('.jpg')
            st = mp4.stat()
            have = game.get(mp4.stem, {})
            if have.get('size') != st.st_size or not (out / 'film' / mp4.name).exists():
                shutil.copyfile(mp4, stage / 'film' / mp4.name)
                if poster.exists():
                    shutil.copyfile(poster, stage / 'film' / poster.name)
                report['game_copied'] = report.get('game_copied', 0) + 1
            seconds = None
            if FFPROBE:
                r = subprocess.run([FFPROBE, '-v', 'error', '-show_entries', 'format=duration', '-of', 'csv=p=0', str(mp4)], capture_output=True, text=True)
                seconds = round(float(r.stdout.strip()), 1) if r.returncode == 0 and r.stdout.strip() else None
            game[mp4.stem] = dict(size=st.st_size, seconds=seconds or have.get('seconds'), filmed=time.strftime('%Y-%m-%d %H:%M', time.localtime(st.st_mtime)))
    report['game'] = len(game)
    for a in assets.values():
        for m in a['models']:
            # a machine is filmed by its form (Skimmer.trial, Skimmer.battle); what the playground builds from a kit of
            # chunks is filmed by its name alone, whatever the board files it under (the Biplane is a vehicle here)
            stems = [f'{a["id"]}.{m["form"]}'] + ([a['id']] if m.get('chunks') or a['category'] == 'building' else [])
            for name, g in game.items():
                stem = next((x for x in stems if name.startswith(x + '.game-')), None)
                if stem is None:
                    continue
                what = name[len(stem) + 6:]
                if not ((stage / 'film' / f'{name}.mp4').exists() or (out / 'film' / f'{name}.mp4').exists()):
                    continue
                m.setdefault('films', []).append(dict(title=GAME_TITLE.get(what, 'In the game: ' + what), file=f'film/{name}.mp4', poster=f'film/{name}.jpg',
                                                      seconds=g.get('seconds'), clips=[], stale=False, what='game', filmed=g.get('filmed'),
                                                      order=GAME_ORDER.index(what) if what in GAME_ORDER else 9))


def earlier_title(folder, stem):
    """'2026-09-28-drive-feel/after' -> 'Earlier · 28 Sep · drive feel, after'."""
    import datetime, re
    m = re.match(r'(\d{4}-\d{2}-\d{2})[-_ ]?(.*)', folder)
    if not m:
        return f'Earlier · {stem.replace("_", " ")}'
    day = datetime.date.fromisoformat(m.group(1))
    rest = re.sub(r'[-_]+', ' ', m.group(2)).replace('/', ', ').strip()
    return f'Earlier · {day.day} {day:%b}' + (f' · {rest}' if rest else '')


def earlier(assets, stage: Path, out: Path, index):
    """The films already on the Drive (src_images links them: drive tests, battle passes, playground rounds) join the
    asset's reel as earlier films, each with a poster cut from it."""
    known = index.setdefault('earlier', {})
    for a in assets.values():
        a['earlier'] = []
        for f in a.get('films', []):
            if not f['link'].startswith('../../films/'):
                continue
            rel = f['link'][len('../../films/'):]
            src = out.parent / 'films' / rel
            name = 'earlier-' + hashlib.sha1(rel.encode()).hexdigest()[:10]
            have = known.get(name, {})
            if not (out / 'film' / f'{name}.jpg').exists() and FFMPEG and src.exists():
                subprocess.run([FFMPEG, '-y', '-loglevel', 'error', '-ss', '2', '-i', str(src), '-frames:v', '1', '-vf', 'scale=640:-2', str(stage / 'film' / f'{name}.jpg')],
                               capture_output=True)
            if 'seconds' not in have and FFPROBE and src.exists():
                r = subprocess.run([FFPROBE, '-v', 'error', '-show_entries', 'format=duration', '-of', 'csv=p=0', str(src)], capture_output=True, text=True)
                have['seconds'] = round(float(r.stdout.strip()), 1) if r.returncode == 0 and r.stdout.strip() else None
            known[name] = have
            folder, stem = Path(rel).parent.as_posix(), Path(rel).stem
            a['earlier'].append(dict(title=earlier_title(folder, stem), file='../films/' + rel, poster=f'film/{name}.jpg', seconds=have.get('seconds'),
                                     clips=[], stale=False, what='earlier', date=f.get('date') or (folder[:10] if folder[:4].isdigit() else ''), form='battle'))
        a['earlier'].sort(key=lambda f: f['date'] or '')


def caption(img, text, counter):
    from PIL import ImageDraw, ImageFont
    d = ImageDraw.Draw(img)
    try:
        big, small = ImageFont.truetype('segoeuib.ttf', 26), ImageFont.truetype('segoeui.ttf', 17)
    except OSError:
        big = small = ImageFont.load_default()
    d.text((22, img.height - 52), text, font=big, fill=BONE)
    if counter:
        w = d.textlength(counter, font=small)
        d.text((img.width - 22 - w, img.height - 44), counter, font=small, fill=DIM)


def encode(frames: Path, result, mp4: Path, poster: Path):
    """Frames to an mp4 that plays in a browser, with the clip names written on; the poster is its first frame."""
    from PIL import Image
    mp4.parent.mkdir(parents=True, exist_ok=True)
    segs = result.get('segments') or []
    named = len(segs) > 1 or (segs and segs[0].get('caption'))
    proc = subprocess.Popen([FFMPEG, '-y', '-loglevel', 'error', '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-s', f'{SIZE}x{SIZE}', '-r', str(FPS), '-i', '-',
                             '-an', '-c:v', 'libx264', '-preset', 'slow', '-crf', '27', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', str(mp4)],
                            stdin=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        for f in range(result['frames']):
            img = Image.open(frames / f'{f:05d}.png').convert('RGB')
            if named:
                k = next((i for i, s in enumerate(segs) if s['first'] <= f < s['first'] + s['count']), None)
                if k is not None:
                    caption(img, segs[k]['caption'], f'{k + 1} / {len(segs)}' if len(segs) > 1 else '')
            if f == 0:
                img.save(poster, quality=84)
            proc.stdin.write(img.tobytes())
        proc.stdin.close()
        err = proc.stderr.read().decode(errors='replace')
        if proc.wait() != 0:
            raise RuntimeError('ffmpeg: ' + err[-300:])
    finally:
        if proc.poll() is None:
            proc.kill()


def attach(P: Path, assets, code, stage: Path, out: Path, local: Path, meta, only_harvest=False):
    jobs = [] if only_harvest else jobs_for(P, assets, code, meta['warnings'])
    cache = local / 'films'
    cache.mkdir(parents=True, exist_ok=True)
    (stage / 'film').mkdir(exist_ok=True)
    index_file = out / 'data' / 'films.json'
    index = json.loads(index_file.read_text(encoding='utf-8')) if index_file.exists() else {}
    if only_harvest:                                  # keep what the last full build made, without rendering anything
        for a in assets.values():
            for m in a['models']:
                m['films'] = [dict(f) for f in index.get('shown', {}).get(f'{a["id"]}.{m["form"]}', []) if f['what'] == 'render' and (out / f['file']).exists()]
    script = (HERE / 'film_blender.py').read_bytes() + (HERE / 'thumb_blender.py').read_bytes()
    todo = []
    for j in jobs:
        spec = {k: v for k, v in j.items() if k not in ('models', 'title', 'order', 'sources')}
        j['key'] = hashlib.sha1(script + json.dumps([thumbs.stamp(j['sources']), spec], sort_keys=True, default=str).encode()).hexdigest()[:16]
        j['file'], j['poster'] = f'film/{j["id"]}.mp4', f'film/{j["id"]}.jpg'
        have = index.get(j['id'], {})
        if have.get('key') == j['key'] and (out / j['file']).exists():
            j['result'] = have
        else:
            j['out'] = (cache / j['id']).as_posix()
            todo.append(j)
    report = meta['sources']['board films'] = dict(blender=thumbs.BLENDER.exists(), ffmpeg=bool(FFMPEG), made=0, reused=len(jobs) - len(todo), failed=[])
    if todo and thumbs.BLENDER.exists() and FFMPEG:
        shares = [todo[k::WORKERS] for k in range(WORKERS) if todo[k::WORKERS]]
        procs = []
        for k, share in enumerate(shares):
            spec, res = cache / f'jobs{k}.json', cache / f'results{k}.json'
            res.unlink(missing_ok=True)
            spec.write_text(json.dumps([{a: b for a, b in j.items() if a not in ('models', 'result', 'sources')} for j in share], default=str), encoding='utf-8')
            procs.append((res, subprocess.Popen([str(thumbs.BLENDER), '-b', '--factory-startup', '-P', str(HERE / 'film_blender.py'), '--', str(spec), str(res)],
                                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)))
        done = {}
        for res, proc in procs:
            proc.wait(timeout=4 * 3600)
            if res.exists():
                done.update(json.loads(res.read_text(encoding='utf-8')))
        for j in todo:
            r = done.get(j['id'], dict(ok=False, error='Blender wrote no result for it'))
            if r.get('ok'):
                try:
                    encode(Path(j['out']), r, stage / j['file'], stage / j['poster'])
                    j['result'] = dict(key=j['key'], ok=True, seconds=round(r['frames'] / FPS, 1), built=meta['built'],
                                       clips=[s['caption'] for s in r.get('segments', []) if s.get('caption')])
                    report['made'] += 1
                except Exception as e:
                    r = dict(ok=False, error=str(e))
            if not r.get('ok'):
                j['result'] = dict(key=None, ok=False, error=r.get('error', 'no film written'))
                report['failed'].append(f'{j["id"]}: {j["result"]["error"]}')
            shutil.rmtree(j['out'], ignore_errors=True)
    elif todo:
        missing = 'Blender' if not thumbs.BLENDER.exists() else 'ffmpeg'
        meta['warnings'].append(f'films: no {missing} on this machine; {len(todo)} films not made (set TW_BLENDER / TW_FFMPEG)')
        for j in todo:
            j['result'] = dict(key=None, ok=False, error=f'no {missing} on this machine')
    for j in sorted(jobs, key=lambda j: j['order']):
        r = j['result']
        index[j['id']] = r if r.get('ok') else index.get(j['id'], r)
        kept = index[j['id']]
        if kept.get('ok') and ((stage / j['file']).exists() or (out / j['file']).exists()):
            for a, m in j['models']:
                m.setdefault('films', []).append(dict(title=j['title'], file=j['file'], poster=j['poster'], seconds=kept.get('seconds'),
                                                      clips=kept.get('clips', []), stale=kept.get('key') != j['key'], what='render', order=j['order']))
    if not only_harvest:
        index['shown'] = {f'{a["id"]}.{m["form"]}': [dict(f) for f in m['films']] for a in assets.values() for m in a['models'] if m.get('films')}
    harvest(P.parents[1], assets, stage, out, index, report)
    earlier(assets, stage, out, index)
    (stage / 'data').mkdir(exist_ok=True)
    (stage / 'data' / 'films.json').write_text(json.dumps(index, indent=1), encoding='utf-8')
    for f in report['failed']:
        meta['warnings'].append('film failed: ' + f)
