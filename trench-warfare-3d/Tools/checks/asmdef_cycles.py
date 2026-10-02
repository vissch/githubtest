WHY = 'The assembly reference graph has no cycle: Unity refuses to compile one, and names only one of its members.'


def run(ctx):
    """Each cycle is reported once, by the path that first closes it. (Walking every path, as this once did, printed
    one line per route into the cycle: 80 MB of them for a single bad reference in TW.Sim.Core.)"""
    asm = ctx.asmdefs
    errors, done, seen = [], set(), set()

    def visit(n, stack):
        if n in stack:
            cycle = stack[stack.index(n):]
            if frozenset(cycle) not in seen:
                seen.add(frozenset(cycle))
                errors.append('cycle: ' + ' -> '.join(cycle + [n]))
            return
        if n in done:
            return
        for r in asm[n][1]:
            if r in asm:
                visit(r, stack + [n])
        done.add(n)

    for n in asm:
        visit(n, [])
    return errors
