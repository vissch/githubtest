WHY = 'A TW.Sim.* assembly references only TW.Sim.*: the sim never sees presentation, UI, net or data code.'


def run(ctx):
    errors = []
    for name, (_, refs) in ctx.asmdefs.items():
        if name.startswith('TW.Sim.'):
            for r in refs:
                if r.startswith('TW.') and not r.startswith('TW.Sim.'):
                    errors.append(f'{name}: sim assembly must not reference {r}')
    return errors
