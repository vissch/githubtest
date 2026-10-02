WHY = 'Every TW.* assembly an asmdef references exists: a typo there is a compile error in every assembly above it.'


def run(ctx):
    errors = []
    names = set(ctx.asmdefs)
    for name, (_, refs) in ctx.asmdefs.items():
        for r in refs:
            if r.startswith('TW.') and r not in names:
                errors.append(f'{name}: unknown reference {r}')
    return errors
