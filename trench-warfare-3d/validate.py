#!/usr/bin/env python3
"""Skeleton validation that runs without Unity: JSON validity, asmdef reference graph (acyclic, sim never
references presentation/UI/net/data), and no UnityEngine usage inside Sim assemblies."""
import json, pathlib, re, sys
root = pathlib.Path(__file__).parent
errors = []

def text(p):
    """Read a source file the way Unity writes one: UTF-8, BOM or not.

    pathlib's read_text() defaults to the locale encoding, which on Windows is cp1252. That decodes most of our files
    by luck and then dies on the first one that holds a character cp1252 has no slot for — a curly quote in a string
    literal was enough to take the whole validator down, and it took the whole team's gate with it. Files are read
    once here and passed around rather than re-read per check, which is also why this used to read some files four
    times over."""
    return p.read_text(encoding='utf-8-sig')
asm = {}
for p in list((root / 'Assets').rglob('*.asmdef')) + [root / 'Packages/manifest.json']:  # Assets only: Library/PackageCache holds package asmdefs
    try:
        d = json.loads(text(p))
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
    txt = text(cs)
    if re.search(r'^\s*using UnityEngine', txt, re.M) or 'UnityEngine.' in txt:
        errors.append(f'{cs}: UnityEngine used inside a Sim assembly')
    if 'Mathf.' in txt or 'Time.deltaTime' in txt or 'System.Random' in txt:
        errors.append(f'{cs}: non-deterministic API in Sim assembly')
# Every C# file under _Project, read once. The checks below all walked the tree separately and re-read each file,
# which was four reads a file and four chances to disagree about what it said.
sources = {cs: text(cs) for cs in (root / 'Assets/_Project').rglob('*.cs')}

# every .cs carries a phase header
for cs, txt in sources.items():
    first = txt.splitlines()[0] if txt.strip() else ''
    if not first.startswith('// Phase:'):
        errors.append(f'{cs}: missing "// Phase:" header')
# brace balance sanity per C# file
for cs, txt in sources.items():
    t = re.sub(r'"(?:\\.|[^"\\])*"', '""', txt)
    t = re.sub(r'//.*', '', t)
    if t.count('{') != t.count('}'):
        errors.append(f'{cs}: unbalanced braces')
# every `using TW.X;` names a namespace that exists. Our assembly names and our namespaces deliberately do NOT
# match (assembly TW.Sim.Core holds namespace TW.Sim, while TW.Sim.Terrain holds TW.Sim.Terrain), so writing the
# assembly name in a using is an easy mistake that costs whoever is sharing the tree a broken compile. A parent
# namespace exists if anything is declared beneath it, which is why this matches on the dotted prefix too.
declared = set()
for txt in sources.values():
    for m in re.finditer(r'^\s*namespace\s+([A-Za-z_][\w.]*)', txt, re.M):
        declared.add(m.group(1))
for cs, txt in sources.items():
    for m in re.finditer(r'^\s*using\s+(TW\.[\w.]*?)\s*;', txt, re.M):
        ns = m.group(1)
        if ns not in declared and not any(d.startswith(ns + '.') for d in declared):
            near = sorted(d for d in declared if d.split('.')[:2] == ns.split('.')[:2])
            errors.append(f'{cs}: `using {ns};` names no namespace that exists'
                          + (f' (did you mean {near[0]}? that is an assembly name, not a namespace)' if near else ''))
print(f'{len(asm)} assemblies, {sum(1 for _ in (root / "Assets/_Project").rglob("*.cs"))} C# files')
if errors:
    print('\n'.join(errors)); sys.exit(1)
print('validation OK')
