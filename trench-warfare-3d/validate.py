#!/usr/bin/env python3
"""Skeleton validation that runs without Unity: JSON validity, asmdef reference graph (acyclic, sim never
references presentation/UI/net/data), and no UnityEngine usage inside Sim assemblies."""
import json, pathlib, re, sys
root = pathlib.Path(__file__).parent
errors = []
asm = {}
for p in list(root.rglob('*.asmdef')) + [root / 'Packages/manifest.json']:
    try:
        d = json.loads(p.read_text())
    except Exception as e:
        errors.append(f'invalid JSON {p}: {e}'); continue
    if p.suffix == '.asmdef':
        asm[d['name']] = (p, d['references'])
names = set(asm)
for name, (p, refs) in asm.items():
    for r in refs:
        if r.startswith('TW.') and r not in names:
            errors.append(f'{name}: unknown reference {r}')
    if name.startswith('TW.Sim.'):
        for r in refs:
            if r.startswith('TW.') and not r.startswith('TW.Sim.'):
                errors.append(f'{name}: sim assembly must not reference {r}')
# cycles
def visit(n, stack):
    if n in stack: errors.append('cycle: ' + ' -> '.join(stack + [n])); return
    for r in asm.get(n, (None, []))[1]:
        if r in asm: visit(r, stack + [n])
for n in asm: visit(n, [])
# UnityEngine inside Sim
for cs in (root / 'Assets/_Project/Sim').rglob('*.cs'):
    txt = cs.read_text()
    if re.search(r'^\s*using UnityEngine', txt, re.M) or 'UnityEngine.' in txt:
        errors.append(f'{cs}: UnityEngine used inside a Sim assembly')
    if 'Mathf.' in txt or 'Time.deltaTime' in txt or 'System.Random' in txt:
        errors.append(f'{cs}: non-deterministic API in Sim assembly')
# every .cs carries a phase header
for cs in (root / 'Assets/_Project').rglob('*.cs'):
    first = cs.read_text().splitlines()[0] if cs.read_text().strip() else ''
    if not first.startswith('// Phase:'):
        errors.append(f'{cs}: missing "// Phase:" header')
# brace balance sanity per C# file
for cs in (root / 'Assets/_Project').rglob('*.cs'):
    t = re.sub(r'"(?:\\.|[^"\\])*"', '""', cs.read_text())
    t = re.sub(r'//.*', '', t)
    if t.count('{') != t.count('}'):
        errors.append(f'{cs}: unbalanced braces')
print(f'{len(asm)} assemblies, {sum(1 for _ in (root / "Assets/_Project").rglob("*.cs"))} C# files')
if errors:
    print('\n'.join(errors)); sys.exit(1)
print('validation OK')
