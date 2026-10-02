"""What the docs say about the assets: owner decisions, open questions, playground rounds, the scale audit."""
import re
from pathlib import Path


def clean(text, limit=420):
    text = re.sub(r'\*\*|`', '', text).strip()
    return text if len(text) <= limit else text[:limit].rsplit(' ', 1)[0] + ' ...'


def attach(repo: Path, assets, warnings):
    rx = {aid: re.compile(a['mention'], re.I if a['category'] == 'character' else 0) for aid, a in assets.items() if a.get('mention')}

    decisions = repo / 'docs/reference/decisions.md'
    if decisions.exists():
        text = decisions.read_text(encoding='utf-8')
        head, _, open_part = text.partition('## Open: waiting on the owner')
        for m in re.finditer(r'^\| (\d{4}-\d{2}-\d{2}) \| (.+?) \|\s*$', head, re.M):
            for aid, r in rx.items():
                if r.search(m.group(2)):
                    assets[aid]['history'].append(dict(date=m.group(1), kind='decision', sha=None, text=clean(m.group(2)), files=[], lane=None))
        open_part = open_part.split('\n## ')[0]
        for bullet in re.split(r'\n(?=- )', open_part):
            if bullet.startswith('- '):
                for aid, r in rx.items():
                    if r.search(bullet):
                        assets[aid]['open_questions'].append(clean(bullet[2:].replace('\n', ' '), 520))
    else:
        warnings.append('docs: no docs/reference/decisions.md')

    playground = repo / 'docs/22-asset-playground.md'
    if playground.exists():
        for section in re.split(r'\n(?=## )', playground.read_text(encoding='utf-8')):
            title = section.split('\n', 1)[0][3:].strip()
            date = re.search(r'\d{4}-\d{2}-\d{2}', title)
            if not date:
                continue
            for aid, r in rx.items():
                n = len(r.findall(section))
                if n >= 2:
                    assets[aid]['history'].append(dict(date=date.group(0), kind='round', sha=None, files=[], lane=None,
                                                       text=f'docs/22-asset-playground.md: "{clean(title, 160)}" ({n} mentions)'))

    scale = repo / 'docs/reference/asset-scale.md'
    if scale.exists():
        block = scale.read_text(encoding='utf-8').split('<!-- gen:asset-scale -->')[-1].split('<!-- /gen:asset-scale -->')[0]
        rows = [[c.strip().strip('`') for c in line.strip().strip('|').split('|')] for line in block.strip().split('\n') if line.startswith('|')]
        if len(rows) > 2:
            cols = rows[0]
            for row in rows[2:]:
                rec = dict(zip(cols, row))
                name = rec.get('Module', '').split('/')[-1]
                if name in assets and len(row) == len(cols):
                    assets[name]['measurements']['Scale audit (seed 1917)'] = (
                        f'{rec.get("Verdict", "?")}: mesh {rec.get("Mesh (m)", "?")} m, placed {rec.get("Placed", "?")} times')
    for a in assets.values():
        a['history'].sort(key=lambda h: (h['date'], h['kind'] != 'commit'), reverse=True)
