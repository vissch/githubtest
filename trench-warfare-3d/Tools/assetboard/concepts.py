#!/usr/bin/env python3
"""The concept page's data: the pictures ComfyUI made for the project, which of them the owner approved, and the
things he asked the board to make.

    python Tools/assetboard/concepts.py                 what the page would show: sessions and counts, approved, requests
    python Tools/assetboard/concepts.py request "text" [--kind vehicle|character|item|other] [--takes 4] [--frog]
    python Tools/assetboard/concepts.py tick            take the open requests one step (ops.py --watch does this itself)

WHY. Hundreds of concept pictures sat in folders on the Drive (the vehicle and mech sets of 2026-10-10/11) and there
was no place to see them by the run that made them, to say "this one", or to ask for more. The page (concepts.html)
shows them, lets the owner approve, and has a box that makes new ones. Approving changes nothing else: the approved
section is an overview, no work follows from it (the owner, 2026-10-15).

A SESSION is one generation run: a set of pictures made together (the first vehicle set, the frog set, mech 60,
mech JP) and "prompted", every picture the page's own box made. The pictures are found on disk (that is the truth of
what exists) and joined to their prompt and seed (the run's manifest) and their idea name (the run's ideas index).
Those two files live in the exploration project outside git (TW_CONCEPT_PROJECT); a picture without them is still shown,
with the name its file gives it.

WHERE THINGS ARE. The pictures: TW_CONCEPT_IMAGES, else the desktop's Drive-synced output folder as the laptop sees it
(G:/Other computers/My Computer/drive). The owner's word: folder `concepts` of the Drive's TW3D-pipeline beside the notes
(TW_CONCEPTS names another): approved.json, and requests/<id>.json, one file per thing he asked for. Thumbnails: the
site's img/concept/. The page cannot write a file, so it sends an approval or a request through the notes listener
(notes.py, /concept/...), which holds the same key.

A REQUEST is made on the desktop: the watcher builds the manifest, finds the broker alive, copies the manifest over and
starts the desktop's gen.py (the Clash LoRA graph of the exploration) through ssh, detached, then counts the pictures
that land in the Drive. States: queued (waiting for the desktop, with the reason), rendering, done, failed (with why),
cancelled. Nothing is retried by itself after a failure: the owner presses Retry.
"""
import argparse
import datetime
import hashlib
import json
import os
import random
import re
import subprocess
import sys
import threading
import time
import urllib.parse
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

IMAGES = Path(os.environ.get('TW_CONCEPT_IMAGES') or 'G:/Other computers/My Computer/drive')
PROJECT = Path(os.environ.get('TW_CONCEPT_PROJECT') or Path.home() / 'Documents' / 'claude' / 'twf3d-vehicle-exploration')
BIG_ROOT = Path(os.environ.get('TW_CONCEPT_BIG') or 'G:/My Drive/TW3D-pipeline/concepts')      # the Drive folder that holds BIG_FOLDER (not the desktop's drive folder)
PROMPTS_DIR = 'G:/My Drive/dieselpunk_iso_compare'
BIG_ROOT_TEXT = str(BIG_ROOT).replace('\\', '/')
# the big sets of vehicles on the Drive's TW3D-pipeline/concepts, each <size>/<NNN_vehicle>/<NNN-k_variant_sS_00001_>.png: a session each. The prompts file keys
# are "<prefix><NNN>/<A|B|C>_s<seed number>" with {prompt, seed}. A set that is still syncing shows what has come.
LOCAL_CLAUDE = (Path.home() / 'Documents' / 'claude').as_posix()
ELEMENT_VERSIONS = {1: 'normal', 2: 'fire', 3: 'ice', 4: 'electric'}      # the four-version rounds (desert, Slavic, sea)
BIGS = [
    dict(sid='vehicles4000', label='5 - Frog vehicles 4000', folder='twf3d_frog_vehicles_4000', prefix='v', prompts=PROMPTS_DIR + '/vehicles4000/vehicles4000_prompts.json',
         about='2026-10-20: 667 frog + dieselpunk vehicles from tiny to giant, three ideas each (plain, frog anatomy, frog species), two seeds each.'),
    dict(sid='pickelhaube300', label='6 - Pickelhaube frogs 300', folder='twf3d_frog_pickelhaube_300', prefix='g', prompts=PROMPTS_DIR + '/pickelhaube_frogs/vehicles_pw_prompts.json',
         about='2026-10-21: 300 Imperial German WW1 machines with the pickelhaube spiked-helmet motif, frog themed; plain, frog anatomy, frog species, two seeds each.'),
    dict(sid='antiair400', label='7 - Pickelhaube anti-air 400', folder='twf3d_frog_antiair_400', prefix='h', prompts=PROMPTS_DIR + '/pickelhaube_antiair/vehicles_aa_prompts.json',
         about='2026-10-21: 400 pickelhaube anti-air machines, frog themed; plain, frog anatomy, frog species, two seeds each.'),
    # the environment trees (the desktop, 2026-11-12): one picture a tree a size, so the prompts file is keyed by the picture's path under the set and the
    # picture is named by its size; the size folders are the categories, biggest first
    dict(sid='trees120', label='13 - Trees 120', folder='twf3d_trees_120', prefix='t', prompts=BIG_ROOT_TEXT + '/twf3d_trees_120/trees_120_prompts.json',
         sizes=['enormous', 'big', 'medium', 'small'], by_path=True,
         about='2026-11-12: 30 stylized casual trees in four sizes (enormous, big, medium, small), local ComfyUI, Krea2 Turbo + Clash LoRA, white background. Stopped at 120 of 300: no logs yet.'),
    # the frog characters (2026-11-12/13, the owner: "put them on the concept page under characters"): infantry and heavy frog humanoids, three prompt variations an idea (ready pose,
    # action, hero) and two seeds each; the folders are the elements (normal, ice, flame, antiair, electric and their mixes). `units` = <elements>/<NNNN_idea>/<NNNN>-<variation>_s<seed>.png,
    # read in place from the desktop's Drive folder (`root`), the prompts file keyed by the path under the set. Stopped by the owner part-way: what exists is shown.
    dict(sid='frogunits', label='14 - Frog characters (infantry + heavy)', folder='twf3d_frog_units', prefix='f', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_frog_units/frog_units_prompts.json',
         units=True, root=IMAGES, shelf='characters',
         about='2026-11-12: frog humanoid infantry and heavy units, dieselpunk WW1, normal / ice / flame / anti-air / electric and their mixes; simplified casual style, portrait, square and landscape, Krea2 Turbo + Clash LoRA. Stopped part-way.'),
    dict(sid='frogcrazy', label='15 - Frog characters, crazy', folder='twf3d_frog_units_crazy', prefix='c', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_frog_units_crazy/frogc_units_prompts.json',
         units=True, root=IMAGES, shelf='characters',
         about='2026-11-12: the crazy round: random surreal mashups (a frog fused with a teapot, a lighthouse, a tuba...), only loosely frog, infantry or heavy, dieselpunk WW1. Stopped part-way.'),
    # the later character rounds (the desktop, 2026-11-13/14, the owner: "make sure all appear in the concept page on the index, per run"): the same `units` layout
    # <group>/<NNNN_idea>/<NNNN>-<version>_s<seed>.png, a set a run. `variants` names the versions (the elemental rounds draw each character four times).
    dict(sid='frogarmor', label='16 - Armoured frog characters', folder='twf3d_frog_armor_1000', prefix='a', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_frog_armor_1000/froga_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-13: stocky armoured frogs with a round hat in the reference look (grey plate, cyborg and sci-fi touches), dieselpunk; the folders are the armour colours. Stopped part-way.'),
    dict(sid='frogelem', label='17 - Frogs: thunder, fire, toxic, heavy, sci-fi', folder='twf3d_frog_armor_elements_500', prefix='e', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_frog_armor_elements_500/frogb_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-13: 500 armoured, super heavy and super sci-fi frogs that are obviously thunder, fire or toxic; the folders are the elements.'),
    dict(sid='reptuni', label='18 - Reptiles, Napoleonic + colonial uniforms', folder='twf3d_reptile_uniforms_1000', prefix='u', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_reptile_uniforms_1000/rept_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-13: lizards, snakes, turtles, geckos and crocodilians in Napoleonic and colonial uniforms and hats. Stopped part-way.'),
    dict(sid='reptarm', label='19 - Reptiles, armour + WW1/WW2', folder='twf3d_reptile_armor_1000', prefix='r', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_reptile_armor_1000/rarm_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-14: the reptiles again with armour (some sci-fi), thunder, fire and toxic themes, and WW1/WW2 clothing; 1800-1960 hats. Stopped part-way.'),
    dict(sid='reptbiome', label='20 - Reptiles by biome (winter, desert, jungle)', folder='twf3d_reptile_biomes_1000', prefix='b', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_reptile_biomes_1000/biom_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-14: winter reptiles with Russian fur hats and warm armour, desert reptiles in khaki colonial, jungle reptiles and amphibians. Stopped part-way.'),
    dict(sid='desert', label='21 - Desert nomads (snakes, rhinos, lizards)', folder='twf3d_desert_nomads_500', prefix='d', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_desert_nomads_500/dsrt_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants=ELEMENT_VERSIONS,
         about='2026-11-14: 500 colonial Arabian desert nomad soldiers, lots of snakes (legless and legged), rhino-horned reptiles and desert lizards; each in four versions. Stopped near the end.'),
    dict(sid='slavic', label='22 - Slavic soldiers (frogs, reptiles, bears)', folder='twf3d_slavic_500', prefix='v', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_slavic_500/slav_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants=ELEMENT_VERSIONS,
         about='2026-11-14: 500 Russian and Eastern European soldiers, mostly frogs, plus reptiles and bears: fur hats, Soviet and Cossack coats, armour and sci-fi; each in four versions.'),
    dict(sid='sea', label='23 - Naval sea creatures', folder='twf3d_sea_500', prefix='n', prompts=str(IMAGES).replace('\\', '/') + '/twf3d_sea_500/sea_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants=ELEMENT_VERSIONS,
         about='2026-11-14: 500 sea-creature sailors and pirates (fish, crustaceans, cephalopods, sea mammals and more): oilskins, captain caps, tricorns, armour and sci-fi; each in four versions.'),
    dict(sid='napoleonic', label='24 - Napoleonic frogs + reptiles', folder='twf3d_napoleonic_500', prefix='m', prompts=LOCAL_CLAUDE + '/krea2-training/bld/nap_units_prompts.json',
         units=True, root=IMAGES, shelf='characters',
         about='2026-11-14: 500 Napoleonic dieselpunk soldiers, mostly frogs plus reptiles: red lancer, cuirassier, dragoon, chevalier guard and bicorne hats, European coats, lots of moustaches; two seeds each.'),
    dict(sid='crabs', label='25 - Naval crabs + crustaceans', folder='twf3d_sea_crabs_500', prefix='k', prompts=LOCAL_CLAUDE + '/krea2-training/bld/crab_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants=ELEMENT_VERSIONS,
         about='2026-11-14: 500 naval crab, lobster, shrimp and other crustacean sailors; each in four versions (normal, fire, ice, electric). Stopped part-way.'),
    dict(sid='darkbirds', label='26 - Dark birds (ravens, crows, raptors)', folder='twf3d_dark_birds_500', prefix='w', prompts=LOCAL_CLAUDE + '/krea2-training/bld/bird_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants=ELEMENT_VERSIONS,
         about='2026-11-14: 500 raven-ish officers in severe black authoritarian-style uniforms, no symbols; each in four versions. Stopped part-way.'),
    dict(sid='artillery', label='27 - Giant frog artillery', folder='twf3d_frog_artillery', prefix='z', prompts='', flat=True, root=IMAGES, shelf='big',
         about='2026-11-14: cartoonishly huge WW1 frog artillery and mobile gun installations, tiny pickelhaube crews at the base; 42 types, 252 images.'),
    dict(sid='veh_desert', label='28 - Desert vehicles (frog, dieselpunk)', folder='twf3d_vehicles_desert_700', prefix='y', prompts=LOCAL_CLAUDE + '/twf3d-vehicle-exploration/tools/veh_desert_prompts.json', flat=True, root=IMAGES,
         about='2026-11-14: colossal desert frog vehicles with microscopic crews, EMTD + Clash LoRA mix. Cancelled part-way.'),
    dict(sid='veh_napoleonic', label='29 - Napoleonic vehicles (frog, dieselpunk)', folder='twf3d_vehicles_napoleonic_700', prefix='y', prompts=LOCAL_CLAUDE + '/twf3d-vehicle-exploration/tools/veh_napoleonic_prompts.json', flat=True, root=IMAGES,
         about='2026-11-14: colossal Napoleonic frog vehicles with microscopic crews. Cancelled part-way.'),
    dict(sid='veh_naval', label='30 - Naval vehicles (frog, dieselpunk)', folder='twf3d_vehicles_naval_700', prefix='y', prompts=LOCAL_CLAUDE + '/twf3d-vehicle-exploration/tools/veh_naval_prompts.json', flat=True, root=IMAGES,
         about='2026-11-14: colossal naval frog vehicles with microscopic crews. Cancelled part-way.'),
    dict(sid='leaders', label='31 - Faction leaders (8 x 200)', folder='twf3d_leaders_200', prefix='l', prompts=LOCAL_CLAUDE + '/krea2-training/bld/lead_units_prompts.json',
         units=True, root=IMAGES, shelf='characters', variants={1: ''},
         about='2026-11-15: 200 royal concepts each of the 8 faction leaders: frog queen, Pickelhaube Kaiser, winter tsar, raven Il Duce, Desert Fox frog, British toad prime minister, salamander Napoleon, crustacean mercenary admiral (lobster, shrimp, crab, crayfish, prawn). One category per leader.'),
    # the owner's 2026-11-15 one-off rounds, a picture a concept straight in the set folder (`loose`); the prompts files, with {prompt, seed, cat, cl, t}, (krea2-training/bld/loose_prompts.py) give each its category
    dict(sid='pickelaa', label='32 - Pickelhaube anti-air frogs', folder='twf3d_pickel_aa', prefix='p', prompts=LOCAL_CLAUDE + '/krea2-training/bld/pickel_aa_prompts.json',
         flat=True, loose=True, root=IMAGES, shelf='characters',
         about='2026-11-15: 200 humanoid pickelhaube frog anti-air soldiers, 8 frog species x 8 anti-air rigs (twin flak, quad AA, shoulder flak, rocket rack, pom-pom, flak arm, balloon gun, searchlight flak). One category per rig.'),
    dict(sid='britww1', label='33 - British WW1 frogs', folder='twf3d_brit_ww1', prefix='q', prompts=LOCAL_CLAUDE + '/krea2-training/bld/brit_ww1_prompts.json',
         flat=True, loose=True, root=IMAGES, shelf='characters',
         about='2026-11-15: 300 very British WW1 frog soldiers in 13 roles: tommy, officer, highlander, sergeant major, trench bomber, Lewis gunner, cavalry, RFC pilot, tank crew, medic, piper, signaller, sniper. One category per role.'),
]
UNIT_NAME = re.compile(r'^(?P<n>\d+)-(?P<i>\d+)_s(?P<s>\d+)$')
UNIT_VARIANT = {1: 'ready pose', 2: 'action', 3: 'hero'}
UNIT_ELEMENT = {'normal': 'Normal', 'ice': 'Ice', 'flame': 'Flame', 'antiair': 'Anti-air', 'electric': 'Electric'}


def unit_category(cat):
    """'ice-electric' -> 'Ice + Electric'."""
    return ' + '.join(UNIT_ELEMENT.get(p, p.replace('_', ' ').title()) for p in cat.split('-'))



def set_root(b):
    """The folder that holds a big set's folder: the Drive's concepts folder, or the set's own `root` (the frog characters are read where the desktop made them)."""
    return Path(b.get('root') or BIG_ROOT)


def root_of(folder_name):
    """The folder that holds the picture folder named `folder_name` (the first part of a picture's id)."""
    for b in BIGS:
        if b['folder'] == folder_name:
            return set_root(b)
    return BIG_ROOT
BIG_SIDS = [b['sid'] for b in BIGS]
# the themed (elemental) versions of the approved units (the desktop, 2026-10-21): each approved unit drawn again as fire, frozen, toxic, electric or plain, in a frog and a
# pickelhaube version, two seeds each, in two rounds. The files are copied to THEMES_DIR (ComfyUI's x_<slot>_<run>_.png) and themes_index.json says which file is which.
THEMES_DIR = Path(os.environ.get('TW_CONCEPT_THEMES') or build.LOCAL / 'themes')
UNITS_DIR = PROMPTS_DIR + '/approved_units'
THEME_SETS = [
    dict(sid='themes1', label='9 - Elemental versions, round 1', folder='approved_themes', units=UNITS_DIR + '/units_with_themed_prompts.json', prompts=UNITS_DIR + '/approved_themes_prompts.json',
         about='2026-10-21: each approved unit again as fire, frozen, toxic, electric or plain, in a frog and a pickelhaube version, two seeds each.'),
    dict(sid='themes2', label='10 - Elemental versions, round 2', folder='approved_themes2', units=UNITS_DIR + '/round2/units_with_round2_prompts.json',
         prompts=UNITS_DIR + '/round2/approved_themes_prompts_round2.json', about='2026-10-21: the same units, the themes drawn harder (round 2).'),
]
# the infantry weapon concepts (the owner, 2026-10-28: "make a weapons section on the concepts page ... for me to approve or not"): one card a weapon, its toy box look
# above its brass-and-walnut upgrade, in BIG_ROOT/<folder>/cards/<group>/<id>.png; index.json says {file under cards: {g, gl, n, t, p, seed, i}}. Made by the weapon
# concepts session (decisions-evidence/2026-10-28/weapons-round3/tools/cards.py). They stand in a section of their own and, approved, on the Weapons shelf.
WEAPON_SETS = [
    dict(sid='weapons', label='12 - Infantry weapons', folder='weapons_round3',
         about='2026-10-28: infantry weapons for every soldier, mostly ones that attack, many with crazy proportions; toy box above, its brass upgrade below.'),
]
WEAPON_SIDS = [w['sid'] for w in WEAPON_SETS]
THEME_WORDS = {'fire': 'Fire', 'freeze': 'Frozen', 'poison': 'Toxic', 'electric': 'Electric', 'normal': 'Plain'}
THEME_KEY = re.compile(r'^i(?P<n>\d+)_(?P<v>frog|pw)_s(?P<s>\d+)$')
BIG_SIZES = ['tiny', 'small', 'medium', 'large', 'giant']
BIG_VARIANT = {1: ('A', 'plain'), 2: ('B', 'frog anatomy'), 3: ('C', 'frog species')}
PROMPTED = 'twf3d_concepts'                      # the folder of IMAGES the page's own requests render into
DESKTOP = 'emtd-desktop'                         # the ssh host of the GPU machine (the owner's ~/.ssh/config)
DESKTOP_DIR = 'C:/Users/PC/Documents/claude/twf3d-vehicle-exploration'      # gen.py and launch.ps1 are there
THUMB = 360                                      # the longest side of a thumbnail, in pixels
THUMB_SECONDS = 25                               # how long a read spends on new thumbnails at most: the first read of 1,600 takes a few of them
RESCAN_SECONDS = 60                              # how often the folders are listed again
RENDER_LIMIT = 60 * 60                           # a request still not done after this long has failed
LOOK_AFTER = 10 * 60                             # and after this long without all its pictures the desktop is asked how it went
IMG = re.compile(r'\.(png|jpe?g|webp)$', re.I)
NAME = re.compile(r'^(?P<n>\d+)-(?P<i>\d+)_(?P<slug>.+?)_s(?P<s>\d+)_(?P<k>\d+)_?$')
PROMPTED_NAME = re.compile(r'^(?P<slug>.+?)_s(?P<seed>\d+)_(?P<k>\d+)_?$')
LOCK = threading.Lock()

# (id, label, folder under IMAGES, manifests, ideas index, about): a session each. The last, "prompted", is not listed here.
SESSIONS = [
    ('first', '1 - First set (plain Clash)', 'twf3d_vehicle_exploration', ['manifest.json', 'manifest_a.json', 'manifest_b.json'], 'ideas_index.json',
     '2026-10-10: nine vehicle categories, Clash LoRA, not frog themed. Paused at about 140 of 960.'),
    ('frog', '2 - Frog set', 'twf3d_frog_vehicle_exploration', ['manifest_frog.json'], 'ideas_index_frog.json',
     '2026-10-11: the same nine categories, frog + dieselpunk/WW1, entirely new ideas.'),
    ('mech60', '3 - Mech 60', 'twf3d_mech60_exploration', ['manifest_mech60.json'], 'ideas_index_mech60.json',
     '2026-10-11: 60 mech types in six slices, dieselpunk and frog themed.'),
    ('mechjp', '4 - Mech JP', 'twf3d_mechjp_exploration', ['manifest_mechjp.json'], 'ideas_index_mechjp.json',
     '2026-10-11: 20 Japanese-style mecha types fused with frog + dieselpunk.'),
]
PROMPTED_SESSION = ('prompted', '11 - Prompted on the board', 'Made from the page\'s own box: what you described, with the Clash LoRA.')

# the words a request's prompt is made of: the LoRA's trigger, the description, then a closing for the kind of thing
TRIGGER = '@clashroyale, 3d render, '
KINDS = {
    'vehicle': ('Vehicle', ', three-quarter view, game unit concept, soft studio lighting, plain light background'),
    'character': ('Character', ', full body, standing pose, three-quarter view, game character concept, soft studio lighting, plain light background'),
    'item': ('Item', ', a single object centred, game item concept, soft studio lighting, plain light background'),
    'other': ('Other', ', game concept art, soft studio lighting, plain light background'),
}
THEMES = {'': '', 'frog': ', frog-themed, dieselpunk World War One style, riveted brass and khaki plate'}
LONGEST_TEXT = 1500
MOST_TAKES = 8


def folder():
    if os.environ.get('TW_CONCEPTS'):
        return Path(os.environ['TW_CONCEPTS'])
    return build.DRIVE / 'concepts' if build.DRIVE.is_dir() else build.LOCAL / 'concepts'


def now_text(now=None):
    return f'{now or datetime.datetime.now():%Y-%m-%d %H:%M:%S}'


def line(s, n=200):
    return re.sub(r'\s+', ' ', str(s or '')).strip()[:n]


def atomic(path: Path, text: str):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(f'{path.name}.{threading.get_ident()}.tmp')
    tmp.write_text(text, encoding='utf-8')
    tmp.replace(path)


def load_json(path: Path, default):
    try:
        return json.loads(Path(path).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def slug_of(text, n=40):
    return re.sub(r'[^a-z0-9]+', '-', str(text).lower()).strip('-')[:n] or 'concept'


def pretty(s):
    return re.sub(r'^\d+_', '', s).replace('-', ' ').replace('_', ' ')


# ---- what the owner approved ----------------------------------------------------------------------------------------

def safe_id(i):
    """An approval names a picture by its path under the images folder: nothing that leaves it, nothing that is not a picture."""
    i = str(i or '').replace('\\', '/')
    if not i or len(i) > 400 or i.startswith('/') or ':' in i or '..' in i.split('/') or not IMG.search(i):
        raise ValueError('that is not a picture of the concepts')
    return i


def approved(where: Path = None):
    d = load_json((where or folder()) / 'approved.json', {})
    return d if isinstance(d, dict) else {}


GROUPS = [('small', 'Small'), ('flying', 'Flying'), ('crab', 'Crab'), ('medium', 'Medium'), ('big', 'Big'), ('mech', 'Mech'), ('weapons', 'Weapons'), ('characters', 'Characters')]     # the shelves of the approved section (the owner, 2026-10-20; Weapons 2026-10-28; Characters 2026-11-13)
FLYING = re.compile(r'ornithopter|flying|rotor|copter|helicopter|balloon|wyvern|quadcopter|floatplane|seaplane|airship|zeppelin|biplane|aircraft|glider|propeller|ducted fan|winged|\bwings?\b|\bkite\b|drone|bomber|\bdrake\b', re.I)
CRAB = re.compile(r'\bcrab', re.I)
MECH = re.compile(r'\bmechs?\b|mecha\b|walker|centaur|biped', re.I)
BIG = re.compile(r'giant|heavy|vast|colossal|huge|mountain|siege|fortress|massive|enormous', re.I)


def classify(it):
    """The shelf a picture starts on: flying, then crab, then mech, then by size. A first guess: the owner moves what is wrong.
    A themed version stands where its approved unit does (`g`)."""
    if it.get('g'):
        return it['g']
    words = re.sub(r'anti-?aircraft', '', f"{it.get('t', '')} {it.get('c', '')}", flags=re.I)       # the type and the category, not the idea's name (a "throat balloon" is no balloon)
    if FLYING.search(words):
        return 'flying'
    if CRAB.search(words):
        return 'crab'
    if it.get('s') in ('mech60', 'mechjp') or str(it.get('c', '')).startswith('mech') or MECH.search(words):
        return 'mech'
    if it.get('s') in BIG_SIDS:
        return {'tiny': 'small', 'small': 'small', 'medium': 'medium'}.get(it.get('c'), 'big')
    if it.get('c') in ('light_vehicles', 'boards'):
        return 'small'
    if BIG.search(words):
        return 'big'
    return 'medium'


def groups(where: Path = None):
    """What the owner put on a shelf himself: {picture id: shelf}. The rest is classify()'s guess."""
    d = load_json((where or folder()) / 'groups.json', {})
    return d if isinstance(d, dict) else {}


def set_group(ids, group, where: Path = None):
    """Put one picture or several on a shelf (group '' takes his word back, so the guess stands). Returns his words. approved.json is not touched."""
    where = where or folder()
    if group and group not in dict(GROUPS):
        raise ValueError('no such shelf: ' + str(group))
    ids = [safe_id(i) for i in (ids if isinstance(ids, list) else [ids])]
    with LOCK:
        d = groups(where)
        for i in ids:
            if group:
                d[i] = group
            else:
                d.pop(i, None)
        atomic(where / 'groups.json', json.dumps(d, indent=1, sort_keys=True))
    return d


def approve(ids, on, where: Path = None, now=None):
    """Approve (or take the approval back from) one picture or several. Returns what is approved now."""
    where = where or folder()
    ids = [safe_id(i) for i in (ids if isinstance(ids, list) else [ids])]
    with LOCK:
        d = approved(where)
        for i in ids:
            if on:
                d.setdefault(i, now_text(now))
            else:
                d.pop(i, None)
        atomic(where / 'approved.json', json.dumps(d, indent=1, sort_keys=True))
    return d


# ---- what he asked for ----------------------------------------------------------------------------------------------

def build_prompt(kind, text, theme=''):
    """The prompt a description becomes: the LoRA's trigger, his words, the theme if asked, the closing for the kind."""
    body = re.sub(r'\s+', ' ', str(text or '')).strip().rstrip('.,;')
    body = re.sub(r'^@clashroyale,?\s*(3d render,?\s*)?', '', body, flags=re.I)
    return TRIGGER + body + THEMES.get(theme or '', '') + KINDS[kind][1]


def requests_dir(where: Path = None):
    return (where or folder()) / 'requests'


def read_requests(where: Path = None):
    out = []
    for f in sorted(requests_dir(where).glob('*.json')) if requests_dir(where).is_dir() else []:
        r = load_json(f, None)
        if isinstance(r, dict) and r.get('id') == f.stem:
            out.append(r)
    return sorted(out, key=lambda r: (r.get('when', ''), r['id']))


def save_request(r, where: Path = None):
    atomic(requests_dir(where) / f'{r["id"]}.json', json.dumps(r, indent=1, ensure_ascii=False))


def request(text, kind='vehicle', title='', takes=4, theme='', prompt='', lora=1.0, where: Path = None, now=None, rng=None):
    """Write a request and return it: queued, with its seeds and the prefix its pictures are saved under."""
    where = where or folder()
    text = str(text or '').strip()
    if len(text) < 8:
        raise ValueError('describe what you want in a few words at least (8 characters)')
    if len(text) > LONGEST_TEXT:
        raise ValueError(f'a description is at most {LONGEST_TEXT} characters')
    if kind not in KINDS:
        raise ValueError(f'the kind is one of {", ".join(KINDS)}')
    if theme not in THEMES:
        raise ValueError('that theme is not one of the page\'s')
    try:
        takes, lora = int(takes), float(lora)
    except (TypeError, ValueError):
        raise ValueError('takes is a whole number and the LoRA strength a number')
    if not 1 <= takes <= MOST_TAKES:
        raise ValueError(f'takes: 1 to {MOST_TAKES}')
    if not 0 <= lora <= 1.5:
        raise ValueError('the LoRA strength is between 0 (off) and 1.5')
    now = now or datetime.datetime.now()
    prompt = str(prompt or '').strip() or build_prompt(kind, text, theme)
    if len(prompt) > 3 * LONGEST_TEXT:
        raise ValueError('the full prompt is too long')
    title = line(title, 60) or line(text, 48)
    rng = rng or random.SystemRandom()
    base = f'c{now:%y%m%d-%H%M%S}'
    rid, n = base, 0
    while (requests_dir(where) / f'{rid}.json').exists():
        n += 1
        rid = f'{base}-{n}'
    seeds = [rng.randrange(100000, 2 ** 31 - 1) for _ in range(takes)]
    r = dict(id=rid, when=now_text(now), kind=kind, title=title, text=text, theme=theme, prompt=prompt, takes=takes, lora=lora, seeds=seeds,
             prefix=f'{PROMPTED}/{kind}/{rid}/{slug_of(title)}', state='queued', why='waiting for the desktop', log=[])
    save_request(r, where)
    return r


def set_state(r, state, why='', where: Path = None):
    r['state'] = state
    r['why'] = line(why, 300)
    r.setdefault('log', []).append(f'{now_text()} {state}' + (f': {line(why, 120)}' if why else ''))
    r['log'] = r['log'][-12:]
    save_request(r, where)
    return r


def change(rid, action, where: Path = None):
    """What the page may do to a request: cancel one that has not started, or retry one that failed or was cancelled."""
    where = where or folder()
    r = next((x for x in read_requests(where) if x['id'] == str(rid)), None)
    if not r:
        raise ValueError(f'{rid}: no such request')
    if action == 'cancel' and r['state'] == 'queued':
        return set_state(r, 'cancelled', 'cancelled by the owner', where)
    if action == 'retry' and r['state'] in ('failed', 'cancelled'):
        r.pop('launched', None)
        r.pop('checked', None)
        return set_state(r, 'queued', 'waiting for the desktop', where)
    raise ValueError(f'{rid} is {r["state"]}: it cannot be {"cancelled" if action == "cancel" else "retried"}')


ROUTES = ('/concept/approve', '/concept/group', '/concept/request', '/concept/cancel', '/concept/retry')      # what the listener passes on (notes.py)


def post(path, d, where: Path = None):
    """The listener's /concept/... routes: what the page sends. Raises ValueError for what it refuses. What changed is in
    the site's state file before this returns."""
    res = route(path, d, where)
    if SITE['out']:
        state_file(SITE['out'], where)
    return res


def route(path, d, where: Path = None):
    if path == '/concept/approve':
        return dict(approved=approve(d.get('ids') or d.get('id'), bool(d.get('approved')), where))
    if path == '/concept/group':
        return dict(own=set_group(d.get('ids') or d.get('id'), d.get('group') or '', where))
    if path == '/concept/request':
        return dict(request=request(d.get('text'), kind=d.get('kind') or 'vehicle', title=d.get('title') or '', takes=d.get('takes') or 4,
                                    theme=d.get('theme') or '', prompt=d.get('prompt') or '', lora=d.get('lora') if d.get('lora') is not None else 1.0, where=where))
    if path in ('/concept/cancel', '/concept/retry'):
        return dict(request=change(d.get('id'), path.rsplit('/', 1)[1], where))
    raise ValueError('nothing here')


# ---- the pictures ---------------------------------------------------------------------------------------------------

def sessions_table():
    return ([dict(id=s[0], label=s[1], about=s[5]) for s in SESSIONS] + [dict(id=b['sid'], label=b['label'], about=b['about']) for b in BIGS]
            + [dict(id=b['sid'], label=b['label'], about=b['about']) for b in THEME_SETS]
            + [dict(id=w['sid'], label=w['label'], about=w['about']) for w in WEAPON_SETS]
            + [dict(id=PROMPTED_SESSION[0], label=PROMPTED_SESSION[1], about=PROMPTED_SESSION[2])])


def listing(root: Path):
    """[(category, type folder, file name)] of a session's folder, in order. A folder that is not there lists nothing."""
    out = []
    if not root.is_dir():
        return out
    for cat in sorted(os.listdir(root)):
        cdir = root / cat
        if cat.startswith('_') or not cdir.is_dir():
            continue
        for typ in sorted(os.listdir(cdir)):
            tdir = cdir / typ
            if not tdir.is_dir():
                continue
            out += [(cat, typ, f) for f in sorted(os.listdir(tdir)) if IMG.search(f)]
    return out


def big_index(root: Path, folder):
    """INDEX.md of a big set: {vehicle number: (size, combat level, movement, shape)}. A missing file gives {}."""
    out = {}
    try:
        text = (root / folder / 'INDEX.md').read_text(encoding='utf-8')
    except OSError:
        return out
    for ln in text.splitlines():
        cells = [c.strip() for c in ln.strip().strip('|').split('|')]
        if len(cells) >= 6 and cells[0].isdigit():
            out[int(cells[0])] = tuple(cells[1:5])
    return out


def scan_units(b, root: Path, given):
    """A frog-character set as items: <elements>/<NNNN_idea>/<NNNN>-<variation>_s<seed>.png, in the order one element, two, three, four (each A-Z), then idea, variation and seed.
    c = the elements folder, t = the idea's name, n = number, name and variation, i = variation, k = seed; `g` puts an approved one on the set's shelf."""
    mine = []
    for cat, typ, f in listing(root / b['folder']):
        m = UNIT_NAME.match(os.path.splitext(f)[0])
        if not m:
            continue
        n, i, k = int(m['n']), int(m['i']), int(m['s'])
        idea = pretty(typ.split('_', 1)[-1]).title()
        row = given.get(f'{cat}/{typ}/{f}', {})
        lab = (b.get('variants') or UNIT_VARIANT).get(i, i)
        elems = '-'.join(sorted(cat.split('-'), key=lambda e: list(UNIT_ELEMENT).index(e) if e in UNIT_ELEMENT else 9))      # 'electric-flame' and 'flame-electric' are one shelf
        mine.append(dict(id=f"{b['folder']}/{cat}/{typ}/{f}", s=b['sid'], c=elems, cl=unit_category(elems), t=idea, n=f"{n:04d} {idea}" + (f" - {lab}" if lab else ''),
                         i=i, k=k, seed=row.get('seed'), p=row.get('prompt') or '', g=b.get('shelf') or ''))
    mine.sort(key=lambda it: (len(it['c'].split('-')), it['c'], it['n'][:4], it['i'], it['k']))
    return mine


def scan_flat(b, root: Path, given):
    """A set with one folder level: <type folder>/<file>.png (the artillery and themed vehicle rounds). c = the type folder, t = its name, n = file stem."""
    mine = []
    d = root / b['folder']
    if not d.is_dir():
        return mine
    if b.get('loose'):
        return scan_loose(b, d, given)
    for cat in sorted(os.listdir(d)):
        cdir = d / cat
        if cat.startswith('_') or not cdir.is_dir():
            continue
        typ = pretty(cat.split('_', 1)[-1]).title()
        for f in sorted(os.listdir(cdir)):
            if not IMG.search(f):
                continue
            stem = os.path.splitext(f)[0]
            m = re.search(r'_s(\d+)', stem)
            row = given.get(f'{cat}/{stem}', {}) or given.get(f'{cat}/{f}', {})
            mine.append(dict(id=f"{b['folder']}/{cat}/{f}", s=b['sid'], c=cat, cl=typ, t=typ, n=f"{cat[:2]} {typ} - {stem.split('_')[0]}", i=0, k=int(m[1]) if m else 1,
                             seed=row.get('seed'), p=row.get('prompt') or '', g=b.get('shelf') or ''))
    return mine


def scan_loose(b, d: Path, given):
    """A set whose pictures lie in its folder itself: <NNN_slug>.png, one a concept. The prompts file, keyed by the file stem, gives
    {prompt, seed, cat, cl, t}: cat (NN_name) is the category, cl its label, t the title. A picture with no row goes under 'other'."""
    mine = []
    for f in sorted(os.listdir(d)):
        if not IMG.search(f) or not (d / f).is_file():
            continue
        stem = os.path.splitext(f)[0]
        row = given.get(stem, {})
        cat = row.get('cat') or '99_other'
        typ = row.get('cl') or pretty(cat.split('_', 1)[-1]).title()
        title = row.get('t') or pretty(stem.split('_', 1)[-1]).title()
        mine.append(dict(id=f"{b['folder']}/{f}", s=b['sid'], c=cat, cl=typ, t=title, n=f"{stem.split('_')[0]} {title}", i=0, k=1,
                         seed=row.get('seed'), p=row.get('prompt') or '', g=b.get('shelf') or ''))
    return mine


def scan_big(root_arg: Path = None):
    """The big sets as items: each in the order tiny..giant, then vehicle, idea and seed. The prompt and seed of each come from its prompts file."""
    items = []
    for b in BIGS:
        root = Path(root_arg) if root_arg else set_root(b)
        given = load_json(Path(b['prompts']), {})
        if b.get('units'):
            items += scan_units(b, root, given)
            continue
        if b.get('flat'):
            items += scan_flat(b, root, given)
            continue
        info = big_index(root, b['folder'])
        mine = []
        for cat, typ, f in listing(root / b['folder']):
            m = NAME.match(os.path.splitext(f)[0])
            if not m:
                continue
            n, i, k = int(m['n']), int(m['i']), int(m['s'])
            letter, label = BIG_VARIANT.get(i, ('', m['slug']))
            if b.get('by_path'):
                label = cat
                row = given.get(f"{cat}/{typ}/{f}", {})
            else:
                row = given.get(f"{b['prefix']}{n:03d}/{letter}_s{k}", {})
            size, level, move, shape = (info.get(n) or (cat, '', '', ''))
            words = ', '.join(w for w in (level, move, shape and shape + ' shape') if w)
            mine.append(dict(id=f"{b['folder']}/{cat}/{typ}/{f}", s=b['sid'], c=cat, cl=cat.capitalize(), t=f'{pretty(typ.split("_", 1)[-1])} ({words})' if words else pretty(typ),
                             n=f'{n:03d} {pretty(typ.split("_", 1)[-1])} - {label}', i=i, k=k, seed=row.get('seed'), p=row.get('prompt') or ''))
        sizes = b.get('sizes') or BIG_SIZES
        mine.sort(key=lambda it: (sizes.index(it['c']) if it['c'] in sizes else 9, it['n'], it['i'], it['k']))
        items += mine
    return items


def scan_themes(by_id=None, root: Path = None):
    """The themed versions as items, in the order of their unit, then round, version and seed. Which file is which comes from themes_index.json; the unit, its theme and the prompt
    from the desktop's files on the Drive. `g` is the shelf of the unit's approved picture, `o` that picture's id."""
    root = Path(root or THEMES_DIR)
    index = load_json(root / 'themes_index.json', {})
    items = []
    for ts in THEME_SETS:
        files = index.get(ts['folder']) or {}
        if not files:
            continue
        units = {u.get('idx'): u for u in load_json(Path(ts['units']), []) if isinstance(u, dict)}
        given = load_json(Path(ts['prompts']), {})
        for f, key in files.items():
            m = THEME_KEY.match(key)
            u = units.get(int(m['n'])) if m else None
            if not u:
                continue
            orig = f"{u['unit']}/{u['file']}"
            name = pretty(str(u['unit']).rsplit('/', 1)[-1].split('_', 1)[-1])
            dmg = u.get('damage') or 'normal'
            base = (by_id or {}).get(orig)
            row = given.get(key, {})
            items.append(dict(id=f"{ts['folder']}/{f}", s=ts['sid'], c=dmg, cl=THEME_WORDS.get(dmg, str(dmg).title()), t=f"{name} ({u.get('role') or 'unit'})",
                              n=f"{int(m['n']):03d} {name} - {'frog' if m['v'] == 'frog' else 'pickelhaube'}", i=int(m['n']), k=int(m['s']), seed=row.get('seed'), p=row.get('prompt') or '',
                              o=orig, g=classify(base) if base else classify(dict(t=name, c=str(u['unit']).split('/')[1] if '/' in str(u['unit']) else '', s=BIG_SIDS[0]))))
    items.sort(key=lambda it: (it['i'], it['s'], it['n'], it['k']))
    return items


def scan_weapons(root: Path = None):
    """The weapon cards as items, in the order of their index (group by group). `g` puts an approved one on the Weapons shelf."""
    root = Path(root or BIG_ROOT)
    items = []
    for w in WEAPON_SETS:
        index = load_json(root / w['folder'] / 'index.json', {})
        for f, d in (index.items() if isinstance(index, dict) else []):
            if not isinstance(d, dict) or not (root / w['folder'] / 'cards' / f).is_file():
                continue
            items.append(dict(id=f"{w['folder']}/cards/{f}", s=w['sid'], c=str(d.get('g') or 'other'), cl=str(d.get('gl') or 'Other'), t=str(d.get('t') or ''),
                              n=str(d.get('n') or pretty(Path(f).stem)), i=int(d.get('i') or 0), k=1, seed=d.get('seed'), p=str(d.get('p') or ''), g='weapons'))
    return items


def source_of(images: Path, item_id):
    """Where a picture's file is: the big sets and the weapons are on the Drive's TW3D-pipeline folder, the others under the images folder."""
    first = str(item_id).split('/')[0]
    if first in [ts['folder'] for ts in THEME_SETS]:
        return THEMES_DIR / item_id
    if first in [b['folder'] for b in BIGS]:
        return root_of(first) / item_id
    return (BIG_ROOT if first in [w['folder'] for w in WEAPON_SETS] else Path(images)) / item_id


MADE = {}      # picture id -> when its file was written: a file does not change, so each is looked at once


def made_at(it, images: Path):
    """When a picture was made, to order the page newest first. The big frog set was copied to the Drive in a minute, so its
    files all say the copy's time: it takes its INDEX.md's time and goes by vehicle number (the run made them in that order)."""
    if it['s'] in BIG_SIDS:
        try:
            return os.stat(root_of(it['id'].split('/')[0]) / it['id'].split('/')[0] / 'INDEX.md').st_mtime
        except OSError:
            return 0.0
    if it['id'] not in MADE:
        try:
            MADE[it['id']] = os.stat(source_of(images, it['id'])).st_mtime
        except OSError:
            return 0.0       # not there (yet): the bottom, and looked at again next read
    return MADE[it['id']]


def scan(images: Path = None, project: Path = None, requests=None):
    """Every concept picture on disk with what is known of it: [item], items in the order of the sessions. An item:
    id (path under the images folder), s (session), c/cl (category and its words), t (type), n (idea name), i (idea
    number), k (take), seed, p (prompt)."""
    images, project = Path(images or IMAGES), Path(project or PROJECT)

    def load(name):
        return load_json(project / name, [])
    items = []
    for sid, _label, folder_name, manifests, index, _about in SESSIONS:
        by_prefix = {}
        for m in manifests:
            for j in load(m):
                by_prefix[str(j.get('prefix', '')).replace('\\', '/')] = j
        ideas = {(i['category'], i['n'], i['idea']): i for i in load(index)}
        for cat, typ, f in listing(images / folder_name):
            stem = os.path.splitext(f)[0]
            m = NAME.match(stem)
            if not m:
                continue
            n, i = int(m['n']), int(m['i'])
            job = by_prefix.get(f'{folder_name}/{cat}/{typ}/{stem.rsplit("_s", 1)[0]}_s{m["s"]}', {})
            info = ideas.get((cat, n, i), {})
            items.append(dict(id=f'{folder_name}/{cat}/{typ}/{f}', s=sid, c=cat, cl=pretty(cat), t=info.get('type') or pretty(typ), n=info.get('name') or pretty(m['slug']),
                              i=i, k=int(m['s']), seed=job.get('seed'), p=job.get('prompt') or info.get('subject') or ''))
    items += scan_big()
    items += scan_themes({it['id']: it for it in items})
    items += scan_weapons()
    asked = {r['id']: r for r in (requests or [])}
    for cat, typ, f in listing(images / PROMPTED):          # <kind>/<request id>/<slug>_s<seed>_00001_.png
        m = PROMPTED_NAME.match(os.path.splitext(f)[0])
        r = asked.get(typ, {})
        kind = cat if cat in KINDS else 'other'
        seed = int(m['seed']) if m else None
        seeds = r.get('seeds') or []
        items.append(dict(id=f'{PROMPTED}/{cat}/{typ}/{f}', s=PROMPTED_SESSION[0], c=kind, cl=KINDS[kind][0], t=r.get('text') or pretty(typ), n=r.get('title') or pretty(m['slug'] if m else f),
                          i=0, k=seeds.index(seed) + 1 if seed in seeds else 1, seed=seed, p=r.get('prompt') or ''))
    items.sort(key=lambda it: (made_at(it, images), it['n'][:4] if it['s'] in BIG_SIDS else '', it['i'], it['k']), reverse=True)       # the last made first (the owner, 2026-10-21)
    return items


def thumb_name(item_id):
    return hashlib.sha1(item_id.encode('utf-8')).hexdigest()[:14] + '.jpg'


def make_thumb(src: Path, dst: Path):
    from PIL import Image
    im = Image.open(src).convert('RGB')
    im.thumbnail((THUMB, THUMB))
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp = dst.with_name(f'{dst.name}.{threading.get_ident()}.tmp')
    im.save(tmp, 'JPEG', quality=78)
    tmp.replace(dst)


def thumbs(items, out: Path, images: Path = None, seconds=THUMB_SECONDS):
    """Make the thumbnails the site does not have yet, for `seconds` at most a read. Returns how many are still missing."""
    images, missing, stop = Path(images or IMAGES), 0, time.time() + seconds
    for it in items:
        dst = out / 'img' / 'concept' / thumb_name(it['id'])
        if dst.exists():
            continue
        if time.time() > stop:
            missing += 1
            continue
        try:
            make_thumb(source_of(images, it['id']), dst)
        except Exception:     # noqa: BLE001  a picture still being written, or one that does not open: next read
            missing += 1
    return missing


# ---- the page's data ------------------------------------------------------------------------------------------------

SCAN = dict(at=0.0, items=[], key='')


def current_items(requests, force=False, now=None):
    """The scan, listed again at most every RESCAN_SECONDS (the folders are on the Drive and are not cheap to list)."""
    now = time.time() if now is None else now
    if force or now - SCAN['at'] >= RESCAN_SECONDS or not SCAN['items']:
        SCAN.update(at=now, items=scan(requests=requests))
    return SCAN['items']


SITE = dict(out=None, items='', missing=0, by_id={})     # where the watcher put the site, the stamp of its pictures and how many thumbnails it still owes


def state_file(out: Path, where: Path = None):
    """data/concepts-state.js: what the page reads again every few seconds. Written on every read and again at once when
    the page changes something, so a page does not take a state older than its own click."""
    where = where or folder()
    reqs = read_requests(where)
    ap = approved(where)
    by = SITE.get('by_id') or {}
    recent = [dict(id=i, n=by[i]['n'], s=by[i]['s'], th=thumb_name(i)) for i, _when in sorted(ap.items(), key=lambda kv: kv[1], reverse=True) if i in by][:8]
    own = groups(where)
    auto = {i: own.get(by[i].get('o')) or classify(by[i]) for i in ap if i in by}       # a themed version stands where its approved unit was put       # the first guess for each approved picture; his own words are in `own`
    top = dict(total=len(by), approved=len(ap), recent=recent)      # what the overview shows of the page: no pictures' list, a few approved ones
    state = dict(top=top, approved=ap, auto=auto, own=own, requests=[{k: r.get(k) for k in ('id', 'when', 'kind', 'title', 'text', 'prompt', 'takes', 'state', 'why', 'theme', 'lora', 'got')} for r in reqs[-30:]],
                 items=SITE['items'], thumbs_missing=SITE['missing'],
                 builder=dict(groups=GROUPS, trigger=TRIGGER, order=list(KINDS), kinds={k: v[1] for k, v in KINDS.items()}, labels={k: v[0] for k, v in KINDS.items()}, themes=THEMES, most=MOST_TAKES))
    write(out / 'data' / 'concepts-state.js', f'window.CONCEPT_STATE = {json.dumps(state, ensure_ascii=False, sort_keys=True)};\n')


def site(out: Path, where: Path = None, force=False, now=None):
    """Put data/concepts.js (the pictures: one script, written when they change) and data/concepts-state.js (what the
    page reads again every few seconds: approved, requests, the stamp of the pictures) in the site. Returns the item count."""
    where = where or folder()
    reqs = read_requests(where)
    items = current_items(reqs, force=force, now=now)
    missing = thumbs(items, out)
    prompts, index = [], {}
    rows = []
    for it in items:
        p = it['p']
        if p not in index:
            index[p] = len(prompts)
            prompts.append(p)
        rows.append([it['id'], it['s'], it['c'], it['cl'], it['t'], it['n'], it['i'], it['k'], it['seed'], index[p], thumb_name(it['id']), it.get('o', ''), it.get('g', '')])
    counts = {s['id']: sum(1 for it in items if it['s'] == s['id']) for s in sessions_table()}
    body = json.dumps(dict(sessions=[dict(s, count=counts[s['id']]) for s in sessions_table()], prompts=prompts, rows=rows,
                           fields='id s c cl t n i k seed p th o g'.split(), root=urllib.parse.quote(str(IMAGES).replace('\\', '/'), safe='/:'),
                           roots=dict({b['folder']: urllib.parse.quote(str(set_root(b)).replace('\\', '/'), safe='/:') for b in BIGS}, **{w['folder']: urllib.parse.quote(str(BIG_ROOT).replace('\\', '/'), safe='/:') for w in WEAPON_SETS}, **{ts['folder']: urllib.parse.quote(str(THEMES_DIR).replace('\\', '/'), safe='/:') for ts in THEME_SETS})), ensure_ascii=False, separators=(',', ':'))
    stamp = hashlib.sha1(body.encode('utf-8')).hexdigest()[:10]
    write(out / 'data' / 'concepts.js', f'window.CONCEPTS = {body};\n')
    SITE.update(out=out, items=stamp, missing=missing, by_id={it['id']: it for it in items})
    state_file(out, where)
    return len(items)


def write(path: Path, text: str):
    if path.exists() and path.read_text(encoding='utf-8') == text:
        return False
    atomic(path, text)
    return True


# ---- making them: the desktop ---------------------------------------------------------------------------------------

def ssh(cmd, timeout=45):
    """One command on the desktop: (exit code, output). The remote shell is Windows PowerShell 5.1."""
    try:
        p = subprocess.run(['ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=8', DESKTOP, cmd], capture_output=True, text=True, timeout=timeout)
        return p.returncode, (p.stdout + p.stderr).strip()
    except (OSError, subprocess.SubprocessError) as e:
        return 255, f'{type(e).__name__}: {e}'


def scp(local: Path, remote_name: str, timeout=45):
    try:
        p = subprocess.run(['scp', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=8', str(local), f'{DESKTOP}:{DESKTOP_DIR}/{remote_name}'], capture_output=True, text=True, timeout=timeout)
        return p.returncode, (p.stdout + p.stderr).strip()
    except (OSError, subprocess.SubprocessError) as e:
        return 255, f'{type(e).__name__}: {e}'


def manifest_of(r):
    """gen.py's manifest of a request: a job a take. The prefix is relative to ComfyUI's output folder, which is the Drive."""
    return [dict(id=f'{r["id"]}_{n + 1}', prompt=r['prompt'], seed=seed, lora=r.get('lora', 1.0), prefix=f'{r["prefix"]}_s{seed}') for n, seed in enumerate(r['seeds'])]


def got(r, images: Path = None):
    """How many of a request's pictures are in the Drive now."""
    d = Path(images or IMAGES) / PROMPTED / r['kind'] / r['id']
    return sum(1 for f in d.iterdir() if IMG.search(f.name)) if d.is_dir() else 0


def wait(r, why, where: Path = None):
    """Stay queued for a reason. The log gets a line when the reason changes, not on every look."""
    if r.get('why') != line(why, 300):
        return set_state(r, 'queued', why, where)
    save_request(r, where)
    return r


def step(r, where: Path = None, run_ssh=ssh, run_scp=scp, images: Path = None, now=None, tmp: Path = None):
    """Take one request one step. queued: the broker answers? then send the manifest and start gen.py. rendering: are
    the pictures there? A request in any other state is left alone. The desktop is only reached by `run_ssh` and
    `run_scp`, so a test hands in its own."""
    where = where or folder()
    now = time.time() if now is None else now
    if r['state'] == 'queued':
        if now - r.get('checked', 0) < 45:
            return r
        r['checked'] = now
        code, said = run_ssh("curl.exe -s -m 5 -o NUL -w '%{http_code}' http://127.0.0.1:8188/system_stats")
        if not said.strip().endswith('200'):          # ssh itself failing is 255; any other code is curl's, through the remote shell
            return wait(r, f'the desktop is not reachable ({line(said, 80)}); waiting' if code == 255 else f'ComfyUI on the desktop is not answering ({line(said, 60) or "no answer"}); waiting', where)
        tmp = tmp or build.LOCAL / 'concepts'
        tmp.mkdir(parents=True, exist_ok=True)
        local, name = tmp / f'manifest_{r["id"]}.json', f'manifest_{r["id"]}.json'
        local.write_text(json.dumps(manifest_of(r), indent=1), encoding='utf-8')
        code, said = run_scp(local, name)
        if code != 0:
            return wait(r, f'the manifest did not reach the desktop ({line(said, 100)}); waiting', where)
        code, said = run_ssh(f'powershell -NoProfile -ExecutionPolicy Bypass -File {DESKTOP_DIR}/launch.ps1 -Manifest {name} -Tag {r["id"]}')
        if code != 0 or 'rc 0' not in said:
            return set_state(r, 'failed', f'gen.py did not start ({line(said, 120)})', where)
        r['launched'] = now
        return set_state(r, 'rendering', f'{r["takes"]} pictures on the desktop', where)
    if r['state'] == 'rendering':
        n = got(r, images)
        r['got'] = n
        if n >= r['takes']:
            return set_state(r, 'done', f'{n} pictures', where)
        waited = now - r.get('launched', now)
        if waited > RENDER_LIMIT:
            return set_state(r, 'failed', f'{n} of {r["takes"]} pictures after {int(waited // 60)} minutes', where)
        if waited > LOOK_AFTER and now - r.get('checked', 0) > 120:
            r['checked'] = now
            code, said = run_ssh(f'Get-Content {DESKTOP_DIR}/render_{r["id"]}.out -Tail 3')
            if code == 0 and 'finished' in said:
                return set_state(r, 'failed', f'the render ended with {n} of {r["takes"]} pictures: {line(said, 160)}', where)
        save_request(r, where)
    return r


WORKER = dict(thread=None)


def tick(where: Path = None, **kw):
    """Take every open request one step, on a thread of its own (an ssh call may take half a minute and a read of the
    board must not wait for it). One pass at a time: a read that finds the last still going does nothing."""
    t = WORKER['thread']
    if t and t.is_alive():
        return False
    where = where or folder()
    if not any(r['state'] in ('queued', 'rendering') for r in read_requests(where)):
        return False

    def work():
        for r in read_requests(where):
            if r['state'] in ('queued', 'rendering'):
                try:
                    step(r, where, **kw)
                except Exception as e:      # noqa: BLE001  one request's trouble must not stop the others
                    print(f'concepts: {r["id"]}: {type(e).__name__}: {e}', flush=True)
    WORKER['thread'] = threading.Thread(target=work, daemon=True)
    WORKER['thread'].start()
    return True


def main(argv=None):
    ap = argparse.ArgumentParser(description='the concept page\'s data')
    ap.add_argument('verb', nargs='?', choices=('list', 'request', 'tick'), default='list')
    ap.add_argument('text', nargs='?', default='')
    ap.add_argument('--kind', default='vehicle', choices=tuple(KINDS))
    ap.add_argument('--takes', type=int, default=4)
    ap.add_argument('--frog', action='store_true', help='frog-themed dieselpunk World War One flavour')
    ap.add_argument('--title', default='')
    a = ap.parse_args(argv)
    where = folder()
    try:
        if a.verb == 'request':
            r = request(a.text, kind=a.kind, title=a.title, takes=a.takes, theme='frog' if a.frog else '', where=where)
            print(f'concepts: {r["id"]} queued, {r["takes"]} takes: {r["prompt"]}')
            return 0
        if a.verb == 'tick':
            reqs = [r for r in read_requests(where) if r['state'] in ('queued', 'rendering')]
            for r in reqs:
                r = step(r, where)
                print(f'concepts: {r["id"]} {r["state"]}: {r.get("why", "")}')
            print(f'concepts: {len(reqs)} open requests')
            return 0
    except ValueError as e:
        print(f'concepts: {e}')
        return 1
    items = scan(requests=read_requests(where))
    print(f'concepts: {len(items)} pictures, {len(approved(where))} approved, in {where}')
    for s in sessions_table():
        print(f'  {s["label"]:34s} {sum(1 for it in items if it["s"] == s["id"])}')
    for r in read_requests(where):
        print(f'  request {r["id"]} {r["state"]:9s} {r["title"]}: {r.get("why", "")}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
