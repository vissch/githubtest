#!/usr/bin/env python3
"""Two shape rules of a brief that briefs.check() applies: a plain title, and a picture for visual work.

WHY. The owner, 2026-11-01, about the Decide page: "right now there is a lot of jargon, i dont know what these titles
for the relays mean. im not sure what they did, what they didnt do. and what is being asked here. I cannot answer if
i dont know what im answering. i need something visual to tell what im deciding on." So:

  title_bad(title, ids)   a title is plain words: no unit id (ability-fx-captures-r2-2-g1), no -g1-g2 or -r3 chain, no
                          dated id, no file path or lane, at most TITLE_MAX characters. `ids` are the ids the brief is
                          about (its unit, its lane); any of them in the title is refused too.
  is_visual(...)          the brief is about visual work: its role is one of VISUAL_ROLES, a file it names is art, an
                          effect, a shader or a capture, or its lane's name says look, fx, death, splash ...
  picture_bad(evidence)   visual work needs at least one picture or film the page can show; --no-evidence does not
                          excuse it (a look nobody can see is not a look he can decide on).

A brief that is not visual is held to the title rule only, and an old plain title passes it. The roles are also the
supervisor's visual set (Tools/supervisor/modules/critic_gate.py reads VISUAL_ROLES from here).
"""
import re
from pathlib import Path

TITLE_MAX = 70
VISUAL_ROLES = ('destruction-vfx-simulator', 'character-simulator', 'env-simulator', 'hard-critic', 'vehicle-simulator')
PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp')
FILMS = ('.mp4', '.webm')
VISUAL_SUFFIXES = PICTURES + FILMS + ('.shader', '.hlsl', '.mat', '.fbx', '.anim', '.controller', '.vfx', '.shadergraph')
VISUAL_DIRS = ('/art/', '/shaders/', '/resources/', 'docs/evidence/', '/presentation/fx', '/presentation/combatfx',
               '/vfx/', '/flipbook')
VISUAL_WORDS = ('look', 'looks', 'fx', 'vfx', 'anim', 'death', 'deaths', 'splash', 'portrait', 'portraits', 'smoke',
                'flame', 'flamer', 'water', 'sprite', 'flipbook', 'shader', 'art', 'flinch', 'muzzle', 'gas', 'night')
_CHAIN = re.compile(r'[a-z0-9]-(?:g|r|c)\d+\b', re.I)                    # sapper-look-3-g2, flamer-look-r2, x-c1
_SLUG = re.compile(r'\b[a-z0-9]+(?:-[a-z0-9]+){2,}\b')                   # three or more lowercase parts: an id
_DATED = re.compile(r'\b20\d{6}\b')                                      # 20261019: a dated id
_PATH = re.compile(r'[/\\]|\b\w+\.(?:png|jpe?g|gif|webp|mp4|webm|cs|py|json|md|shader|mat|fbx|ps1|txt|log)\b', re.I)


def one(s):
    return re.sub(r'\s+', ' ', str(s or '')).strip()


def title_bad(title, ids=()):
    """Why a title is not plain words: every reason, or [] when it is."""
    t, bad = one(title), []
    if len(t) > TITLE_MAX:
        bad.append(f'the title takes {len(t)} characters; {TITLE_MAX} at most, in plain words')
    if _PATH.search(t):
        bad.append('the title names a file, a folder or a lane: say in plain words what it is about')
    if _CHAIN.search(t):
        bad.append('the title carries a unit id chain (-g1, -r2, -c1): say in plain words what the work is')
    elif _SLUG.search(t):
        bad.append(f'"{_SLUG.search(t).group(0)}" in the title reads as an id: say in plain words what the work is')
    if _DATED.search(t):
        bad.append('the title carries a dated id: say in plain words what the work is')
    low = t.lower()
    for i in ids or ():
        i = one(i).lower()
        if i.startswith(('lane/sim/', 'lane/show/')):
            i = i.split('/', 2)[2]
        if i and ('-' in i or any(c.isdigit() for c in i)) and i in low:
            bad.append(f'the title names the id {i}: say in plain words what the work is')
            break
    return bad


_TAIL = re.compile(r'(?:-(?:g|r|c)\d+|-\d+|-20\d{6})$', re.I)


def id_words(uid):
    """A unit id in words: ability-fx-captures-r2-2-g1 -> 'ability fx captures'."""
    s = one(uid).lower()
    while _TAIL.search(s):
        s = _TAIL.sub('', s)
    return re.sub(r'[-_]+', ' ', s).strip()


def fit(title, ids=()):
    """A title a program wrote, made plain: a path becomes its file's name, an id its words, and it is cut to
    TITLE_MAX at a word. For writers that run with nobody to answer a refusal (the loop's park brief, found.py).
    A title that is plain already comes back as it is."""
    if not title_bad(title, ids):
        return one(title)
    out = []
    for tok in one(title).split():
        core_, end = re.match(r'(.*?)([.,;:!?)]*)$', tok).groups()
        if _PATH.search(core_):
            core_ = Path(core_.replace('\\', '/').rstrip('/')).stem or core_
            core_ = re.sub(r'^.*/', '', core_)
        if _CHAIN.search(core_) or _SLUG.search(core_) or _DATED.search(core_) or any(
                one(i) and one(i).lower().split('/')[-1] == core_.lower() for i in ids or ()):
            core_ = id_words(core_) or core_
        out.append(core_ + end)
    t = ' '.join(w for w in ' '.join(out).split())
    if len(t) > TITLE_MAX:
        t = t[:TITLE_MAX + 1].rsplit(' ', 1)[0].rstrip('.,;:')
    return t[:1].upper() + t[1:]


def is_visual(role='', lane='', files=(), kind=''):
    """Whether a brief is about visual work (see the module's text)."""
    if one(role) in VISUAL_ROLES or kind == 'concepts':
        return True
    for f in files or ():
        s = '/' + str(f).replace('\\', '/').lower()
        if s.endswith(VISUAL_SUFFIXES) or any(d in s for d in VISUAL_DIRS):
            return True
    lane = one(lane).lower()
    if not lane.startswith('lane/show/'):             # a sim lane is never a look, whatever its name
        return False
    return any(w in VISUAL_WORDS for w in re.split(r'[^a-z]+', lane[len('lane/show/'):]))


def picture_bad(evidence):
    """[] when one piece of evidence is a picture or a film; else the reason."""
    if any(Path(str(p)).suffix.lower() in PICTURES + FILMS for p, _ in evidence or ()):
        return []
    return ['it is about visual work and shows no picture: give at least one still or film of it (--evidence); '
            '--no-evidence does not excuse a look']
