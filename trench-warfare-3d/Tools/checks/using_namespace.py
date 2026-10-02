import re

WHY = 'Every `using TW.X;` names a namespace that exists: assembly names and namespaces differ here on purpose.'


def run(ctx):
    """Our assembly names and our namespaces deliberately do NOT match (assembly TW.Sim.Core holds namespace TW.Sim,
    while TW.Sim.Terrain holds TW.Sim.Terrain), so writing the assembly name in a using is an easy mistake that costs
    whoever is sharing the tree a broken compile. A parent namespace exists if anything is declared beneath it, which
    is why this matches on the dotted prefix too."""
    errors = []
    declared = set()
    for txt in ctx.sources.values():
        for m in re.finditer(r'^\s*namespace\s+([A-Za-z_][\w.]*)', txt, re.M):
            declared.add(m.group(1))
    for cs, txt in ctx.sources.items():
        for m in re.finditer(r'^\s*using\s+(TW\.[\w.]*?)\s*;', txt, re.M):
            ns = m.group(1)
            if ns not in declared and not any(d.startswith(ns + '.') for d in declared):
                near = sorted(d for d in declared if d.split('.')[:2] == ns.split('.')[:2])
                errors.append(f'{cs}: `using {ns};` names no namespace that exists'
                              + (f' (did you mean {near[0]}? that is an assembly name, not a namespace)' if near else ''))
    return errors
