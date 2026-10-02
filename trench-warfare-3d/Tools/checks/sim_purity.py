import re

WHY = 'No UnityEngine and no non-deterministic API under Sim/: the sim must give the same hash on every machine.'


def run(ctx):
    errors = []
    sim = ctx.root / 'Assets/_Project/Sim'
    for cs, txt in ctx.sources.items():
        if sim not in cs.parents:
            continue
        if re.search(r'^\s*using UnityEngine', txt, re.M) or 'UnityEngine.' in txt:
            errors.append(f'{cs}: UnityEngine used inside a Sim assembly')
        if 'Mathf.' in txt or 'Time.deltaTime' in txt or 'System.Random' in txt:
            errors.append(f'{cs}: non-deterministic API in Sim assembly')
    return errors
