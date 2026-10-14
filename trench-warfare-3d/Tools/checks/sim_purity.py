import re

WHY = ('No UnityEngine and no non-deterministic API under Sim/, Net/ or Data/: the sim must give the '
       'same hash on every machine.')

# Caught everywhere in scope (Sim/, Net/, Data/): no legitimate deterministic use exists today.
# Not caught today: a bare `Math.X` after `using System;` (only the qualified `System.Math.` is in the list).
# It is the same hazard, and no file under Sim/, Net/ or Data/ uses it, so widening is safe -- but it changes
# what this check catches, so it belongs to a finding of its own.
DENYLIST = (
    'MathF.', 'System.Math.', 'Time.deltaTime', 'System.Random', 'DateTime.', 'Stopwatch',
    'FloatMode.Fast', 'System.Linq', 'using System.Linq',
)
# Caught only under Sim/: a hash-state container (Dictionary/HashSet/NativeHashMap) or a Mathf call
# both have reasoned-about, deterministic uses already in Net/ and Data/ (pooled lookup dictionaries,
# baked constants) that are not part of SimWorld.Hash() — widening these two to Net/Data would flag
# that existing code, not a bug; see decisions.md.
SIM_ONLY_DENYLIST = ('Mathf.', 'Dictionary<', 'HashSet<', 'NativeHashMap')
MATH_FN = re.compile(r'\bmath\.(sin|cos|pow|exp)\s*\(')
NEW_RANDOM = re.compile(r'\bnew\s+Random\s*\(\s*\)')  # the unseeded System.Random ctor; Unity.Mathematics.Random always takes a seed


def run(ctx):
    errors = []
    sim = ctx.root / 'Assets/_Project/Sim'
    det_roots = [sim, ctx.root / 'Assets/_Project/Net', ctx.root / 'Assets/_Project/Data']
    for cs, txt in ctx.sources.items():
        in_sim = sim in cs.parents
        in_det_scope = any(root in cs.parents for root in det_roots)
        if not in_det_scope:
            continue
        if in_sim and (re.search(r'^\s*using UnityEngine', txt, re.M) or 'UnityEngine.' in txt):
            errors.append(f'{cs}: UnityEngine used inside a Sim assembly')
        hit = (any(tok in txt for tok in DENYLIST) or (in_sim and any(tok in txt for tok in SIM_ONLY_DENYLIST))
               or MATH_FN.search(txt) or NEW_RANDOM.search(txt))
        if hit:
            errors.append(f'{cs}: non-deterministic API in Sim assembly')
    return errors
