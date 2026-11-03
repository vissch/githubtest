#!/usr/bin/env python3
"""A brief's body built from a relay unit: what it was for, what was done, before and after side by side, the critic's
score, and two or three options. The supervisor's modules call it when a unit needs the owner (the critic gate's hold,
the families cap, the landing), so every such brief reads the same way.

WHY. The owner, 2026-11-01, about the Decide page: "right now there is a lot of jargon, i dont know what these titles
for the relays mean. im not sure what they did, what they didnt do. and what is being asked here. I cannot answer if
i dont know what im answering. i need something visual to tell what im deciding on."

    import brief_card
    c = brief_card.card(unit, kind='held')      # a dict: title, what_for, done, options, why, evidence, role, lane ...
    b = brief_card.post(where, c)                # briefs.add() with it: its shape rules apply (plain title, a picture)

`unit` is a dict: id, lane, role, goal (what it was for, in words), report (the last leg's report: its RESULT and
CHANGED lines are what was done), before and after (lists of still paths, matched in order), score and fixes (the
critic's), and optionally title (plain words) and run/leg (the relay run that raised it). `kind` picks the options:
'held' (the critic held it), 'landing' (it passed and can land), 'family' (a chain of fixes reached its cap).
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import briefrules  # noqa: E402

BAR = 70                    # the owner, 2026-11-02: a visual unit passes with a critic score of 70 or more
FOR_WORDS, OPTION_WORDS, WHY_WORDS, CAPTION_WORDS, MOST_EVIDENCE = 45, 22, 35, 14, 6   # briefs.py's limits
OPTIONS = {
    'held': ['Make the critic\'s three fixes, then show me again',
             'Take it as it is: it is good enough for now',
             'Drop this work: it is not worth more spend'],
    'landing': ['Land it', 'Send it back: I say below what is wrong'],
    'family': ['Stop this chain of fixes here and show me the best one',
               'Allow one more round of fixes',
               'Drop the work: it is not getting better'],
}


def one(s):
    return re.sub(r'\s+', ' ', str(s or '')).strip()


def cut(s, n):
    w = one(s).split()
    return ' '.join(w[:n]).rstrip('.,;:') + ('...' if len(w) > n else '')


def plain_name(unit):
    """The unit in plain words: its title, else its id without the -r2-g1 chain and the date, hyphens as spaces."""
    t = one(unit.get('title'))
    if t and not briefrules.title_bad(t):
        return t
    s = briefrules.id_words(unit.get('id')) or 'this work'
    return s[0].upper() + s[1:]


def title_of(unit, kind):
    name = plain_name(unit)
    tail = {'held': f': the critic scored it {unit.get("score")}, below {BAR}' if unit.get('score') is not None
            else ': the critic could not score it',
            'landing': ': ready to land', 'family': ': the fixes reached their cap'}[kind]
    t = name + tail
    while len(t) > briefrules.TITLE_MAX and ' ' in name:
        name = name.rsplit(' ', 1)[0]
        t = name + tail
    return t[:briefrules.TITLE_MAX]


def done_of(report):
    """What was done, from a leg report: its RESULT line and its CHANGED bullets, ids and paths kept out of the
    first line the page shows."""
    out, on = [], False
    for l in str(report or '').splitlines():
        s = l.strip()
        if s.startswith('RESULT:'):
            out.append(s[len('RESULT:'):].strip())
        elif s.startswith('CHANGED'):
            on = True
        elif on and s.startswith('-'):
            out.append(s.lstrip('- ').strip())
        elif on and s and not s.startswith('-'):
            on = False
    return [o for o in out if o][:5]


def side_by_side(before, after, dst):
    """One picture of before (left) and after (right), or None when Pillow is missing or a file does not read."""
    try:
        from PIL import Image
        with Image.open(before) as a, Image.open(after) as b:
            h = min(a.height, b.height, 720)
            a = a.convert('RGB').resize((max(1, round(a.width * h / a.height)), h))
            b = b.convert('RGB').resize((max(1, round(b.width * h / b.height)), h))
            im = Image.new('RGB', (a.width + b.width + 8, h), (255, 255, 255))
            im.paste(a, (0, 0))
            im.paste(b, (a.width + 8, 0))
            Path(dst).parent.mkdir(parents=True, exist_ok=True)
            im.save(dst)
        return Path(dst)
    except Exception:           # no Pillow, or a still it cannot read: the two stills are shown one by one instead
        return None


def evidence_of(unit, work=None):
    """[(path, caption)]: each before/after pair side by side when it can be drawn, else the two one by one; after
    stills with no before alone. At most six."""
    before = [str(p) for p in unit.get('before') or []]
    after = [str(p) for p in unit.get('after') or []]
    out = []
    for i, a in enumerate(after):
        name = re.sub(r'[-_]+', ' ', Path(a).stem)
        if i < len(before):
            both = side_by_side(before[i], a, Path(work) / f'pair-{i + 1}.png') if work else None
            if both:
                out.append((str(both), cut(f'Before (left) and after (right): {name}', CAPTION_WORDS)))
            else:
                out += [(before[i], cut(f'Before: {name}', CAPTION_WORDS)), (a, cut(f'After: {name}', CAPTION_WORDS))]
        else:
            out.append((a, cut(f'After: {name}', CAPTION_WORDS)))
    return out[:MOST_EVIDENCE]


def card(unit, kind='held', options=None, work=None):
    """The brief's body as a dict (see the module's text). `options` replaces the kind's own; `work` is a folder for
    the side-by-side pictures."""
    if kind not in OPTIONS:
        raise ValueError(f'kind is one of {", ".join(OPTIONS)}')
    score, fixes = unit.get('score'), [one(f) for f in unit.get('fixes') or [] if one(f)][:3]
    goal = one(unit.get('goal')) or plain_name(unit)
    said = (f'The relay passed it; the critic scored it {score}/100, and a look passes at {BAR}.' if score is not None
            else 'The relay passed it; the critic could not score its stills.')
    what_for = cut(f'{cut(goal, FOR_WORDS - 22)}. {said}', FOR_WORDS)
    if kind == 'held':
        why = cut(f'Below {BAR} it does not count as done. The critic\'s first fix: {fixes[0]}' if fixes
                  else f'Below {BAR} it does not count as done.', WHY_WORDS)
    elif kind == 'landing':
        why = cut(f'It passed its own check and the critic scored it {score}/100.', WHY_WORDS)
    else:
        why = cut(f'The fixes went round without reaching {BAR}; best so far {score}/100.', WHY_WORDS)
    return dict(title=title_of(unit, kind), what_for=what_for, done=done_of(unit.get('report')),
                options=[cut(o, OPTION_WORDS) for o in (options or OPTIONS[kind])], why=why,
                evidence=evidence_of(unit, work), score=score, fixes=fixes, role=one(unit.get('role')),
                lane=one(unit.get('lane')), unit=one(unit.get('id')), kind=kind,
                raised_by=dict(run=unit['run'], unit=one(unit.get('id')), leg=int(unit.get('leg') or 0))
                if unit.get('run') else None)


def text(c):
    """The card as the page's words, for a log or a note: one block."""
    lines = [c['title'], '', 'What it was for: ' + c['what_for'], 'What was done:']
    lines += ['  - ' + d for d in c['done']] or ['  (the report says nothing)']
    lines += ['Critic: %s/100' % c['score'] if c['score'] is not None else 'Critic: no score']
    lines += ['  fix: ' + f for f in c['fixes']]
    lines += ['Pictures:'] + ['  %s  (%s)' % (p, cap) for p, cap in c['evidence']]
    lines += ['Options:'] + ['  %s. %s' % ('ABCD'[i], o) for i, o in enumerate(c['options'])]
    return '\n'.join(lines)


def post(where, c, by='supervisor', now=None):
    """Write the card as a brief (briefs.add: its shape rules apply) and keep the card's own parts in it."""
    import briefs
    b = briefs.add(Path(where), c['title'], c['what_for'], c['options'], c['why'], c['evidence'], lane=c['lane'],
                   by=by, now=now, raised_by=c['raised_by'], role=c['role'])
    b['card'] = dict(done=c['done'], score=c['score'], fixes=c['fixes'], kind=c['kind'])
    briefs.put(Path(where) / b['id'], b)
    return b
