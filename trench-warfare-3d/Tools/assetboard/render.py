"""Write the pages: the overview, one page per asset, the process page. jinja2, autoescaped (commit subjects and
notes carry < and &). No script framework: the overview's filters are a few lines in static/site.js."""
import shutil
from pathlib import Path

import model

HERE = Path(__file__).resolve().parent
CATEGORY = {'character': 'Characters', 'vehicle': 'Vehicles', 'building': 'Buildings'}
BLURB = {
    'FINAL': 'A model of its own, drawn by the game, and fielded or placed.',
    'IN_PROGRESS': 'No battle model of its own yet, but someone is on it: trial art in the playground, a live lane, or an open board stage.',
    'NEEDS_VISUAL': 'The sim knows it and the game draws it with another unit\'s model. Nothing is in progress.',
    'READY_UNUSED': 'Modelled and drawn, but no faction fields it and no level places it.',
    'IDEA': 'Planned only: no id in the code, no files.',
}
KIND = {'commit': 'changed', 'mention': 'named', 'lane': 'on a lane', 'lane-mention': 'named on a lane', 'decision': 'decision', 'round': 'round'}


def hero(a):
    """The picture that stands for the asset, and what it is."""
    own = [m for m in a['models'] if m.get('thumb') and m['form'] == 'battle']
    trial = [m for m in a['models'] if m.get('thumb') and m['form'] == 'trial']
    if own and not a['drawn_as']:
        return own[0]['thumb'], 'Blender preview of the battle model'
    if trial:
        return trial[0]['thumb'], 'Blender preview of the trial model (playground)'
    if own:
        return own[0]['thumb'], 'Blender preview'
    pics = [i for i in a['images'] if i['kind'] == 'unitart'] or [i for i in a['images'] if i['kind'] == 'portrait' and not a['pictures'].get('placeholder')]
    if pics and not a['drawn_as']:
        return pics[0]['file'], pics[0]['caption']
    return None, ''


def reel(a):
    """An asset's films in the order they are shown: a turntable, the game's own films, then the Blender renders."""
    out = [dict(f, form=m['form']) for m in a['models'] for f in m.get('films', [])]
    out.sort(key=lambda f: (f['what'] != 'game', f['form'] != 'battle', f.get('order', 0)))
    out += a.get('earlier', [])                       # what was filmed before, oldest first, after what it is now
    turn = next((f for f in out if f['file'].endswith(('.game-turn.mp4', '.turn.mp4'))), None)
    own = next((f for f in out if f['what'] == 'game' and f['file'].startswith(f'film/{a["id"]}.')), None)
    if turn and own and not turn['file'].startswith(f'film/{a["id"]}.'):
        turn = own                                    # a unit drawn with a shared figure opens on its own game film
    if turn:                                          # a page and a card open on the model going round
        out.remove(turn)
        out.insert(0, turn)
    return out


def facts(a):
    """The numbers of an asset, as a few chips."""
    out = []
    for m in a['models']:
        tris = next((l.get('tris') for l in m.get('lods', []) if l.get('tris')), None)
        if tris:
            out.append((f'triangles ({m["form"]})', f'{tris:,}'))
        if m['form'] != 'procedural' and len(m.get('lods', [])) > 1 and a['category'] == 'vehicle':
            out.append(('LODs', len(m['lods'])))
        if m.get('chunks'):
            out.append(('chunks', m['chunks']))
        if m.get('sockets'):
            out.append(('sockets', len(m['sockets'])))
    rows = a.get('vfx', {}).get('rows') or []
    if rows:
        out.append(('effects drawn', f'{sum(1 for r in rows if r["ok"])} of {len(rows)}'))
    if a.get('measurements', {}).get('Size (m)'):
        out.append(('m', a['measurements']['Size (m)']))
    if a['tests']:
        out.append(('tests', len(a['tests'])))
    return out


def site(assets, code, extra, lanes, meta, stage: Path):
    from jinja2 import Environment, FileSystemLoader, select_autoescape
    env = Environment(loader=FileSystemLoader(str(HERE / 'templates')), autoescape=select_autoescape(['html']), trim_blocks=True, lstrip_blocks=True)
    env.globals.update(STATUS_LABEL=model.STATUS_LABEL, CATEGORY=CATEGORY, BLURB=BLURB, KIND=KIND, meta=meta)
    order = {s: i for i, s in enumerate(model.STATUSES)}
    cat_order = {c: i for i, c in enumerate(CATEGORY)}
    rows = sorted(assets.values(), key=lambda a: (order[a['status']], cat_order[a['category']], a['notes'].get('priority', 9),
                                                  a['archetype'] if a['archetype'] is not None else 999, a['id']))
    for a in rows:
        a['hero'], a['hero_what'] = hero(a)
        a['stand_in'] = None
        if a['drawn_as'] and a['drawn_as'] in assets:
            a['stand_in'] = next((m.get('thumb') for m in assets[a['drawn_as']]['models'] if m.get('thumb')), None)
        elif a['drawn_as']:        # a figure (Soldier) that several units share: any unit made for it carries its preview
            a['stand_in'] = next((m.get('thumb') for o in assets.values() if o.get('figure') == a['drawn_as'] and not o['drawn_as']
                                  for m in o['models'] if m.get('thumb')), None)
        a['reel'], a['facts'], a['stand_reel'] = reel(a), facts(a), []
        if not a['reel'] and a['drawn_as']:           # the films of the model it borrows, shown as borrowed
            lender = assets.get(a['drawn_as']) or next((o for o in assets.values() if o.get('figure') == a['drawn_as'] and not o['drawn_as']), None)
            a['stand_reel'] = reel(lender) if lender else []
        if a['reel']:
            a['hero'], a['hero_what'] = a['reel'][0]['poster'], a['reel'][0]['title']
        a['looks'] = [dict(l, form=m['form']) for m in a['models'] for l in m.get('looks', [])]
        why = a['status_why'].split(';')[0].split(', lane/')[0]
        a['why_short'] = why + (f' · {len(a["lanes"])} lanes name it' if 'lane' in a['status_why'] and len(a['lanes']) > 1 else '')
        a['live_lanes'] = [l for l in a['lanes'] if l['live'] and l['touches_art']]
        a['other_lanes'] = [l for l in a['lanes'] if not (l['live'] and l['touches_art'])]
        days = {}
        for h in a['history']:
            days.setdefault(h['date'], []).append(h)
        a['days'] = sorted(days.items(), reverse=True)
        a['changes'] = sum(1 for h in a['history'] if h['kind'] in ('commit', 'lane'))
    sections = [(s, [a for a in rows if a['status'] == s]) for s in model.STATUSES]
    levels = sorted({t for a in rows for t in a['level_tags']})

    funnel = []
    for cat in CATEGORY:
        group = [a for a in rows if a['category'] == cat and a['kind'] in ('unit', 'house', 'structure')]   # the ladders of the units and the buildings
        steps = []
        for a in group:
            for s in a['stages']:
                if s['label'] not in [x['label'] for x in steps]:
                    steps.append(dict(label=s['label'], key=s['key'], ok=0, of=0))
        for step in steps:
            have = [next((s for s in a['stages'] if s['label'] == step['label']), None) for a in group]
            step['of'] = sum(1 for s in have if s)
            step['ok'] = sum(1 for s in have if s and s['ok'])
        funnel.append(dict(category=CATEGORY[cat], total=len(group), steps=steps))

    (stage / 'a').mkdir(parents=True, exist_ok=True)
    for f in (HERE / 'static').iterdir():
        shutil.copyfile(f, stage / f.name)
    (stage / 'index.html').write_text(env.get_template('index.html').render(sections=sections, levels=levels, total=len(rows), root=''), encoding='utf-8')
    live = [l for l in lanes if l['live']]
    parked = [l for l in lanes if not l['live']]
    (stage / 'process.html').write_text(env.get_template('process.html').render(
        funnel=funnel, sections=sections, live=live, parked=parked, board=extra.get('board', []), orphans=extra.get('orphan_events', []),
        events=model.EVENT_ASSET, root=''), encoding='utf-8')
    page = env.get_template('asset.html')
    for a in rows:
        (stage / 'a' / f'{a["id"]}.html').write_text(page.render(a=a, assets=assets, root='../'), encoding='utf-8')
