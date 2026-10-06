"""The asset records: what each character, vehicle and building has, how far it has come, and which bucket it is in.

Pure functions over the tables src_code and src_files read: no git, no images. The rules are the board's meaning,
so they are written out here and tested (test_assetboard.py).

STATUS, evaluated in this order:
  IDEA          only in the notes file: no id in the code, no files
  FINAL         has a model of its own, the game draws it, and it is fielded (a faction slot or pool; a building:
                placed on a ground or used on the Home Front)
  READY_UNUSED  a model of its own, drawn, fielded nowhere
  IN_PROGRESS   no battle model of its own yet, and trial art in the playground, a live lane touching its art, or
                an open board stage
  NEEDS_VISUAL  the sim knows it, it has no model of its own, and nothing is in progress
"""
import json
import re
from pathlib import Path

import src_code
import src_files

STATUSES = ['FINAL', 'IN_PROGRESS', 'NEEDS_VISUAL', 'READY_UNUSED', 'IDEA']
STATUS_LABEL = {'FINAL': 'Final, in the battle', 'IN_PROGRESS': 'In progress', 'NEEDS_VISUAL': 'Needs a visual',
                'READY_UNUSED': 'Ready, not used', 'IDEA': 'Idea'}
NOTE_KEYS = {'priority', 'note', 'wants', 'model_of', 'parked'}
WANTS = {'model', 'portrait', 'vfx', 'level'}

# What a socket on a machine's model is for (TankRenderer.OnSimEvent, TankRenderer.Lights.cs, TankRenderer.Salvo.cs).
SOCKET_EFFECTS = [
    ('Muzzle flash, smoke and dust wings', ('Socket_Muzzle', 'Socket_HullMG')),
    ('Rocket salvo, one trail a tube', ('Socket_Tube',)),
    ('Fire and smoke when it burns', ('Socket_Fire',)),
    ('Exhaust smoke and glow', ('Socket_Exhaust',)),
    ('Dust and bog spurts', ('Socket_Dust', 'Socket_Toe')),
    ('Crew bailing out', ('Socket_Crew',)),
    ('Gas vent', ('Socket_Vent',)),
]
# Sim events that belong to one unit: when nothing in Presentation or UI reads the event, that unit's moment is not drawn.
EVENT_ASSET = {'LeapStarted': 'Jetpack', 'ShieldBlocked': 'Shield', 'UnitHealed': 'Medic', 'VehicleHullMended': 'Repair',
               'DropInbound': 'Para', 'DropLanded': 'Para', 'BreakerPhase': 'Breaker', 'SapperLaying': 'Sapper',
               'SapperOrdered': 'Sapper', 'RocketFired': 'Salvo'}
# Words a commit subject or a doc uses for a character (their ids alone are too common as words).
MENTION = {'Rifle': r'riflem[ae]n', 'Assault': r'assault (?:trooper|infantry|m[ae]n)', 'Machinegunner': r'machine[- ]gunners?|MG team',
           'Sniper': r'snipers?', 'Officer': r'officers?', 'Shield': r'shield bearers?', 'Medic': r'medics?', 'Repair': r'engineers?',
           'Para': r'paratroopers?|paras\b', 'Jetpack': r'jetpack', 'Frog': r'frog (?:soldier|infantry|rifleman|m[ae]n)|FigureFrog|frogrig',
           'Sentry': r'sentry', 'AtRifle': r'anti-tank rifle|AtRifle', 'DeathBattalion': r'death battalion', 'Sapper': r'sappers?',
           'Flamethrower': r'flamethrower (?:unit|man|trooper)'}


class NotesError(Exception):
    pass


def load_notes(path: Path):
    notes = json.loads(path.read_text(encoding='utf-8')) if path.exists() else {}
    for key in ('assets', 'aliases'):
        notes.setdefault(key, {})
    for key in ('planned', 'ignore'):
        notes.setdefault(key, [])
    for aid, n in notes['assets'].items():
        bad = set(n) - NOTE_KEYS
        if bad:
            raise NotesError(f'{path.name}: {aid} has unknown keys {sorted(bad)} (allowed: {sorted(NOTE_KEYS)})')
        if n.get('wants') and n['wants'] not in WANTS:
            raise NotesError(f'{path.name}: {aid} wants "{n["wants"]}", which is not one of {sorted(WANTS)}')
    return notes


def stage(key, label, ok, evidence=''):
    return dict(key=key, label=label, ok=bool(ok), evidence=evidence)


def has(asset, key):
    return any(s['ok'] for s in asset['stages'] if s['key'] == key)


def new_asset(aid, category, name=None, **kw):
    a = dict(id=aid, category=category, kind=kw.pop('kind', 'unit'), name=name or aid, archetype=None, blurb='', aliases=[aid],
             drawn_as=None, models=[], stages=[], status=None, status_why='', vfx={}, levels=[], level_tags=[], tests=[],
             lanes=[], board=[], history=[], images=[], measurements={}, notes={}, roots=[], pictures={}, open_questions=[],
             mention=None, films=[])
    a.update(kw)
    return a


def fielding(code, name):
    slots = [f for f, s in code['slots'].items() if name in s]
    pools = [f for f, p in code['pools'].items() if name in p and f not in slots]
    return slots, pools


def unit_levels(code, name, library):
    levels, tags = [], []
    slots, pools = fielding(code, name)
    grounds = list(code['grounds'])
    if 'Iron' in slots or 'Brass' in slots:
        levels.append(dict(kind='campaign', text=f'Campaign: all {len(code["missions"])} missions (Iron against Brass), on {", ".join(grounds)}'))
        tags += grounds
    others = [f for f in slots if f not in ('Iron', 'Brass')]
    if others:
        levels.append(dict(kind='faction', text='Fielded by ' + ', '.join(slots) + ' (ten roster slots each)'))
    if pools:
        levels.append(dict(kind='pool', text='In the pool of ' + ', '.join(pools) + ': drops and captures, not a roster slot'))
    if name in code['defined']:
        levels.append(dict(kind='sandbox', text='Unit Sandbox and Proving Ground only (defined in UnitDefinitions.All, in no faction)'))
        tags.append('Sandbox')
    if name in library:
        levels.append(dict(kind='playground', text='Playground scene (asset trial)'))
        tags.append('Playground')
    return levels, tags


def machine_vfx(models, drawn_as):
    drawn = next((m for m in models if m['form'] == 'battle'), None)
    sockets = drawn['sockets'] if drawn else []
    rows = []
    for label, prefixes in SOCKET_EFFECTS:
        hit = [s for s in sockets if s.startswith(prefixes)]
        rows.append(dict(effect=label, ok=bool(hit), detail=', '.join(hit) if hit else 'no socket for it'))
    roles = {}
    for part in (drawn['parts'] if drawn else []):
        role = re.sub(r'(_[LR]\w*|\d+)$', '', part)
        roles[role] = roles.get(role, 0) + 1
    note = ''
    if not drawn:
        note = f'No battle model of its own: in a battle it uses the {drawn_as}\'s sockets and effects.' if drawn_as else 'No battle model.'
    return dict(rows=rows, roles=roles, note=note,
                generic='Armour hits, sparks, cook-off, thrown parts, lamps and the wreck are generic code for every machine.')


def build(P: Path, code: dict, notes: dict):
    """Every asset the code and the files know, with its ladder. Lanes, board, history and images are added later."""
    assets = {}
    pics = src_files.pictures(P)
    library = src_files.library(P)
    crabs = src_files.load(P / 'Resources/Vehicles/crabs.json')['crabs'] if (P / 'Resources/Vehicles/crabs.json').exists() else {}
    figures, rule = code['figures'], code['figure_rule']
    subscribed = src_code.mentions(P, ['Presentation', 'UI'], r'SimEventType\.(\w+)')
    read_events = set()
    for cs in list((P / 'Presentation').rglob('*.cs')) + list((P / 'UI').rglob('*.cs')):
        read_events |= set(re.findall(r'SimEventType\.(\w+)', src_code.read(cs)))
    orphans = [e for e in code['events'] if e not in read_events]

    def picture_stage(a, pname):
        portrait = pics['portraits'].get(pname)
        a['pictures'] = dict(portrait=portrait, unitart=pics['unitart'].get(pname), moods=pics['moods'].get(pname, []),
                             placeholder=pname in pics['placeholders'])
        real = portrait and pname not in pics['placeholders']
        why = ('UI/Skin/Portraits/' + pname + '.png') if real else ('a generated placeholder' if portrait else 'no portrait')
        return stage('picture', 'Portrait', real, why)

    def unit_common(a, name, cls):
        a['archetype'], a['blurb'] = code[cls][name]
        pname = code['portraits'].get(name, name)
        a['name'] = pname if pname != 'Vehicle' else name
        a['aliases'] = sorted({name, pname} | set(notes['aliases'].get(name, [])))
        slots, pools = fielding(code, name)
        a['levels'], a['level_tags'] = unit_levels(code, name, library)
        sim = name in code['shipped'] or name in code['defined']
        where = 'RosterEntry.ForArchetype' if name in code['shipped'] else 'UnitDefinitions.All' if sim else 'an id, no definition'
        return sim, where, pname, slots, pools

    # ---- characters
    for name in code['infantry']:
        a = new_asset(name, 'character')
        sim, where, pname, slots, pools = unit_common(a, name, 'infantry')
        figure = figures[rule['by'].get(a['archetype'], rule['other'])]
        own = figure == name or notes['assets'].get(name, {}).get('model_of') == figure
        own_figure = figure if own else name
        a['models'] = src_files.figure_models(P, own_figure)
        battle = own and any(m['form'] == 'battle' for m in a['models'])
        trial = any(m['form'] == 'trial' for m in a['models'])
        if not own:
            a['drawn_as'] = figure
        a['figure'] = figure
        atlas = P / 'Resources/Units' / f'Figure{own_figure}Atlas.bytes'
        a['stages'] = [
            stage('sim', 'In the sim', sim, where),
            stage('trial', 'Trial figure (playground)', trial, f'Playground/Art/Units/{name}/frogrig.json' if trial else ''),
            stage('model', 'Battle figure of its own', battle, f'Resources/Units/Figure{own_figure}*' if battle else f'drawn as the {figure}'),
            stage('drawn', 'Drawn by the game', battle and own_figure in figures, 'VATRenderer.FigureNames' if battle else ''),
            stage('animated', 'Animated (baked clips)', battle and atlas.exists(), f'Figure{own_figure}Atlas.bytes' if battle and atlas.exists() else ''),
            picture_stage(a, pname),
            stage('tested', 'Tested', False),
            stage('fielded', 'Fielded', slots or pools, ', '.join(slots + [p + ' (pool)' for p in pools])),
        ]
        a['mention'] = MENTION.get(name, re.escape(name))
        a['roots'] = [f'Playground/Art/Units/{name}/', f'UI/Skin/Portraits/{pname}.png', f'UI/Resources/UnitArt/{pname}.png',
                      f'UI/Resources/UnitArt/States/{pname}_']
        if own:
            a['roots'] += [f'Art/Characters/{own_figure}', f'Resources/Units/Figure{own_figure}']
        events = [e for e, who in EVENT_ASSET.items() if who == name and e in code['events']]
        a['vfx'] = dict(rows=[dict(effect=f'Its own moment: {e}', ok=e not in orphans,
                                   detail='drawn' if e not in orphans else 'the sim raises it, nothing in Presentation or UI reads it') for e in events],
                        roles={}, note='' if battle else f'Shares the {figure}\'s figure, so every effect is the {figure}\'s.',
                        generic='Muzzle flash, tracer, deaths (shot, blast, gas, crushed, burning, beam), bodies and gibs are generic code for every infantryman.')
        assets[name] = a

    # ---- vehicles
    for name in code['vehicle']:
        a = new_asset(name, 'vehicle')
        sim, where, pname, slots, pools = unit_common(a, name, 'vehicle')
        a['models'] = src_files.vehicle_models(P, name, crabs)
        battle = any(m['form'] == 'battle' for m in a['models'])
        trial = any(m['form'] == 'trial' for m in a['models'])
        row = code['machines'].get(name)
        drawn = bool(row) or name in ('Maw', 'Tusk')
        if not (battle and drawn):
            a['drawn_as'] = 'Tusk' if name == 'Tusk' else 'Maw'
        model = next((m for m in a['models'] if m['form'] == 'battle'), None)
        a['stages'] = [
            stage('sim', 'In the sim', sim, where),
            stage('trial', 'Trial model (playground)', trial, f'Playground/Art/Tanks/{name}/tank3.json' if trial else ''),
            stage('library', 'In the playground library', name in library, 'PlaygroundLibrary.asset' if name in library else ''),
            stage('model', 'Battle model of its own', battle, f'Resources/Vehicles/{name}/' if battle else f'drawn as the {a["drawn_as"]}'),
            stage('drawn', 'Drawn by the game', battle and drawn, 'TankRenderer.Machines row' if row else 'TankRenderer.ModelFor' if drawn else ''),
            stage('sockets', 'Effect sockets', bool(model and model['sockets']), f'{len(model["sockets"])} sockets' if model and model['sockets'] else ''),
            picture_stage(a, pname),
            stage('tested', 'Tested', False),
            stage('fielded', 'Fielded', slots or pools, ', '.join(slots + [p + ' (pool)' for p in pools])),
        ]
        a['mention'] = r'\b' + re.escape(name) + r'\b'
        a['roots'] = [f'Resources/Vehicles/{name}/', f'Resources/Vehicles/{name}Atlas', f'Playground/Art/Tanks/{name}/',
                      f'UI/Skin/Portraits/{pname}.png', f'UI/Resources/UnitArt/{pname}.png', f'UI/Resources/UnitArt/States/{pname}_']
        if name in ('Maw', 'Tusk'):
            a['roots'].append('Resources/Vehicles/TankAtlas')
        a['vfx'] = machine_vfx(a['models'], a['drawn_as'])
        for e, who in EVENT_ASSET.items():
            if who == name and e in code['events']:
                a['vfx']['rows'].append(dict(effect=f'Its own moment: {e}', ok=e not in orphans,
                                             detail='drawn' if e not in orphans else 'the sim raises it, nothing in Presentation or UI reads it'))
        if model and model.get('size_m'):
            a['measurements']['Size (m)'] = ' x '.join(f'{v:.1f}' for v in model['size_m'])
        assets[name] = a

    # ---- machines that are not units: any model folder with no archetype, and the two the terrain code draws
    sea = [g for g, f in code['grounds'].items() if f['sea']]
    for folder in sorted((P / 'Resources/Vehicles').iterdir()):
        if folder.is_dir() and folder.name not in assets:
            a = new_asset(folder.name, 'vehicle', kind='extra')
            a['models'] = src_files.vehicle_models(P, folder.name, crabs)
            users = src_code.mentions(P, ['Presentation'], r'"' + folder.name + r'"')
            pname = folder.name
            a['stages'] = [stage('model', 'Battle model of its own', bool(a['models']), f'Resources/Vehicles/{folder.name}/'),
                           stage('drawn', 'Drawn by the game', bool(users), ', '.join(users)),
                           stage('sockets', 'Effect sockets', bool(a['models'] and a['models'][0]['sockets']), ''),
                           picture_stage(a, pname), stage('tested', 'Tested', False),
                           stage('fielded', 'Placed', bool(users), 'on grounds with a sea: ' + ', '.join(sea))]
            a['blurb'] = 'Not a unit: no archetype, drawn by terrain code.'
            a['levels'], a['level_tags'] = [dict(kind='ground', text='Grounds with a sea: ' + ', '.join(sea))], list(sea)
            a['mention'] = r'\b' + re.escape(folder.name) + r'\b'
            a['roots'] = [f'Resources/Vehicles/{folder.name}/', f'Resources/Vehicles/{folder.name}Atlas']
            a['vfx'] = machine_vfx(a['models'], None)
            assets[folder.name] = a

    all_houses = src_files.houses(P, code['building_sets'])
    grounds_all = list(code['grounds'])
    craft = P / 'Presentation/Terrain/LandingCraftView.cs'
    if craft.exists():
        a = new_asset('LandingCraft', 'vehicle', 'Landing craft', kind='extra')
        a['blurb'] = 'Not a unit: the boats that bring reinforcements ashore. A procedural mesh, no model file.'
        a['models'] = [dict(form='procedural', lods=[dict(lod=0, path=src_files.rel(craft, P), tris=None)], texture=None, manifest=None,
                            parts=[], sockets=[], size_m=None)]
        a['stages'] = [stage('model', 'Model of its own', True, 'a procedural mesh built in LandingCraftView.cs'),
                       stage('drawn', 'Drawn by the game', True, 'LandingCraftView.cs'), stage('tested', 'Tested', False),
                       stage('fielded', 'Placed', bool(sea), 'on grounds with a sea: ' + ', '.join(sea))]
        a['levels'], a['level_tags'] = [dict(kind='ground', text='Grounds with a sea: ' + ', '.join(sea))], list(sea)
        a['mention'] = r'landing craft'
        a['roots'] = ['Presentation/Terrain/LandingCraftView.cs']
        a['vfx'] = dict(rows=[], roles={}, note='', generic='Wake and beaching are drawn by LandingCraftView itself.')
        assets['LandingCraft'] = a
    if 'biplane' in code['kit_fields'] and 'Biplane' in all_houses:
        h = all_houses['Biplane']
        placed_by = code['placed_fields'].get('biplane')
        a = new_asset('Biplane', 'vehicle', kind='extra', set=h['set'])
        a['blurb'] = 'Not a unit: the strafing aircraft, and a crashed landmark on the field.'
        tex = P / 'Resources/Env' / h['set'] / f'{h["set"]}.jpg'
        a['models'] = [dict(form='battle', lods=[dict(lod=0, path=f'Resources/Env/{h["set"]}/Biplane.fbx', tris=h['tris'])],
                            texture=src_files.rel(tex, P) if tex.exists() else None, manifest=h['manifest'], parts=[], sockets=[], size_m=None,
                            chunks=h['chunks'], whole=f'Resources/Env/{h["set"]}/Biplane.fbx')]
        a['stages'] = [stage('model', 'Model cut and imported', True, h['manifest']),
                       stage('destructible', 'Breaks chunk by chunk', h['chunks'] > 1, f'{h["chunks"]} chunks'),
                       stage('drawn', 'In the battlefield kit', True, 'BattlefieldKit.cs'), stage('tested', 'Tested', False),
                       stage('fielded', 'Placed on a ground', placed_by, f'{placed_by}: {", ".join(grounds_all)}' if placed_by else '')]
        a['levels'], a['level_tags'] = [dict(kind='ground', text='A landmark on ' + ', '.join(grounds_all))], list(grounds_all)
        a['mention'] = r'\b[Bb]iplane\b'
        a['roots'] = [f'Resources/Env/{h["set"]}/Biplane.fbx', f'Resources/Env/{h["set"]}/Chunks/Biplane_']
        a['vfx'] = dict(rows=[dict(effect='Destruction, chunk by chunk', ok=True, detail=f'{h["chunks"]} chunks')], roles={}, note='',
                        generic='The strafe run\'s tracers and dust are the ability\'s effects, not the model\'s.')
        a['chunk_rows'] = h['rows']
        assets['Biplane'] = a

    # ---- buildings: the chunked houses of the building sets, and the Siege structures the kit imports whole
    home = {}
    for b in code['home_front']:
        home.setdefault(b['model'], []).append(b)
    grounds = list(code['grounds'])
    river = [g for g, f in code['grounds'].items() if f['river']]
    building_sets = [s for s in code['building_sets'] if s in ('Houses', 'Military', 'Ruins', 'Siege')]
    names = [(h, d['set']) for h, d in all_houses.items() if d['set'] in building_sets]
    names += [(n, s) for field, (s, n) in code['kit_fields'].items() if s == 'Siege' and n not in all_houses]
    for name, set_name in names:
        if name in notes['ignore']:
            continue
        a = new_asset(name, 'building', kind='house' if set_name != 'Siege' else 'structure', set=set_name)
        h = all_houses.get(name)
        whole = P / 'Resources/Env' / set_name / f'{name}.fbx'
        field = next((f for f, (s, n) in code['kit_fields'].items() if n == name), None)
        in_kit = set_name in code['building_sets'] and (h is not None) or field is not None
        placed_by = code['placed_sets'].get(set_name) if h and set_name != 'Siege' else code['placed_fields'].get(field)
        where = river if set_name == 'Houses' else grounds
        tex = P / 'Resources/Env' / set_name / f'{set_name}.jpg'
        a['models'] = [dict(form='battle', lods=[dict(lod=0, path=f'Resources/Env/{set_name}/', tris=h['tris'] if h else None)],
                            texture=src_files.rel(tex, P) if tex.exists() else None, manifest=h['manifest'] if h else None,
                            parts=[], sockets=[], size_m=None, chunks=h['chunks'] if h else 0, whole=src_files.rel(whole, P) if whole.exists() else None)]
        a['stages'] = [
            stage('model', 'Model cut and imported', h or whole.exists(), h['manifest'] if h else src_files.rel(whole, P) if whole.exists() else ''),
            stage('destructible', 'Breaks chunk by chunk', h and h['chunks'] > 1, f'{h["chunks"]} chunks' if h else 'one piece'),
            stage('drawn', 'In the battlefield kit', in_kit, 'BattlefieldKit.cs'),
            stage('fielded', 'Placed on a ground', placed_by, f'{placed_by}: {", ".join(where)}' if placed_by else 'no composer places it'),
            stage('homefront', 'Used on the Home Front', name in home, ', '.join(b['name'] for b in home.get(name, []))),
            stage('tested', 'Tested', False),
        ]
        if placed_by:
            a['levels'].append(dict(kind='ground', text='Placed on ' + ', '.join(where) + (' (needs a river and a bridge)' if set_name == 'Houses' else '')))
            a['level_tags'] += where
        if name in home:
            a['levels'].append(dict(kind='homefront', text='Home Front: ' + ', '.join(f'{b["name"]} ({b["faction"]})' for b in home[name])))
            a['level_tags'].append('HomeFront')
        a['mention'] = r'\b' + re.escape(name) + r'\b'
        a['roots'] = [f'Resources/Env/{set_name}/{name}_', f'Resources/Env/{set_name}/Chunks/{name}_', f'Resources/Env/{set_name}/{name}.fbx']
        mats = ', '.join(f'{n} {m}' for m, n in sorted(h['mats'].items())) if h else ''
        a['vfx'] = dict(rows=[dict(effect='Destruction, chunk by chunk', ok=bool(h and h['chunks'] > 1),
                                   detail=f'{h["chunks"]} chunks ({mats}); each has its own hit points' if h else 'a single piece'),
                              dict(effect='Lamps and chimney smoke (Home Front stages)', ok=name in home, detail='FactionBuildings stages' if name in home else 'none')],
                        roles={}, note='', generic='Bullet wear and ramming by heavy machines are generic. No burning or scorch code exists for buildings.')
        if h:
            a['measurements'].update({'Chunks': h['chunks'], 'Triangles': h['tris'], 'Vertices': h['verts']})
            a['chunk_rows'] = h['rows']
        assets[name] = a

    # ---- tests that name each asset
    naming = src_files.tests_naming(P, [a for a in assets])
    for aid, a in assets.items():
        a['tests'] = [dict(file=f, hits=n) for f, n in naming.get(aid, [])]
        for s in a['stages']:
            if s['key'] == 'tested':
                s['ok'] = bool(a['tests'])
                s['evidence'] = ', '.join(Path(t['file']).stem for t in a['tests'][:4]) + (' ...' if len(a['tests']) > 4 else '')
        tris = {f'{m["form"]} LOD{l["lod"]}': l['tris'] for m in a['models'] for l in m['lods'] if l.get('tris') and a['category'] != 'building'}
        if tris:
            a['measurements']['Triangles'] = ', '.join(f'{k} {v}' for k, v in tris.items())
        a['code'] = src_code.mentions(P, ['Presentation', 'UI'], r'Archetype\.' + re.escape(aid) + r'\b') if a['kind'] == 'unit' else {}

    # ---- the notes file: per-asset notes, and assets that are only an idea
    for aid, n in notes['assets'].items():
        if aid not in assets:
            raise NotesError(f'asset-notes.json: "{aid}" is not an asset the code or the files know. The known ids: {", ".join(sorted(assets))}')
        assets[aid]['notes'] = n
    for idea in notes['planned']:
        if idea['id'] in assets:
            raise NotesError(f'asset-notes.json: planned "{idea["id"]}" already exists in the code or the files: move its note to "assets"')
        a = new_asset(idea['id'], idea['category'], idea.get('name'), kind='idea')
        a['blurb'] = idea.get('note', '')
        a['notes'] = {k: idea[k] for k in ('priority', 'source') if k in idea}
        a['mention'] = r'\b' + re.escape(idea.get('name', idea['id'])) + r'\b'
        assets[idea['id']] = a
    return assets, dict(orphan_events=orphans, library=library)


def lane_only_asset(aid, category, lane):
    """An asset whose files exist only on a lane branch (a model folder integration does not have yet)."""
    a = new_asset(aid, category, kind='lane')
    a['blurb'] = f'Exists only on {lane}: integration has no id or file for it yet.'
    a['mention'] = r'\b' + re.escape(aid) + r'\b'
    sub = 'Tanks' if category == 'vehicle' else 'Units'
    a['roots'] = [f'Resources/Vehicles/{aid}/', f'Resources/Vehicles/{aid}Atlas', f'Playground/Art/{sub}/{aid}/', f'Resources/Units/Figure{aid}']
    return a


def decide(a):
    """Set status and status_why from the ladder, the lanes and the board."""
    live = [l for l in a['lanes'] if l['live'] and l['touches_art']]
    open_board = [b for b in a['board'] if b['state'] in ('IN_PROGRESS', 'READY', 'RECHECK', 'STALE')]
    if a['kind'] == 'idea':
        a['status'], a['status_why'] = 'IDEA', 'planned in the notes file; no id in the code, no files'
        return a
    if a['kind'] == 'lane':
        a['status'], a['status_why'] = 'IN_PROGRESS', 'its files exist only on ' + ', '.join(l['branch'] for l in a['lanes'])
        return a
    own = has(a, 'model') and has(a, 'drawn')
    reach = has(a, 'fielded') or has(a, 'homefront')
    if own and reach:
        a['status'] = 'FINAL'
        a['status_why'] = 'own model, drawn by the game, ' + next(s['evidence'] for s in a['stages'] if s['key'] in ('fielded', 'homefront') and s['ok'])
        if live:
            a['status_why'] += '. Being reworked on ' + ', '.join(l['branch'] for l in live)
    elif own:
        a['status'], a['status_why'] = 'READY_UNUSED', 'own model, drawn by the game, but ' + (
            'no composer places it and the Home Front does not use it' if a['category'] == 'building' else 'in no faction slot or pool')
    elif not a['notes'].get('parked') and (has(a, 'trial') or live or open_board):
        why = []
        if has(a, 'trial'):
            why.append('trial art in the playground')
        if live:
            why.append('live lane ' + ', '.join(l['branch'] for l in live))
        if open_board:
            why.append('board item ' + ', '.join(f'{b["item"]}/{b["stage"]} {b["state"]}' for b in open_board))
        a['status'], a['status_why'] = 'IN_PROGRESS', '; '.join(why) + ('; no battle model of its own yet' if a['drawn_as'] else '')
    else:
        no_image = not (a['pictures'].get('portrait') or a['pictures'].get('unitart'))
        a['status'] = 'NEEDS_VISUAL'
        a['status_why'] = (f'drawn as the {a["drawn_as"]}' if a['drawn_as'] else 'no model') + ('; no image at all' if no_image else
                           '; its portrait is a generated placeholder' if a['pictures'].get('placeholder') else '; has a portrait')
    return a
