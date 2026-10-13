WHY = 'Every TW.* assembly an asmdef references exists: a typo there is a compile error in every assembly above it.'


def run(ctx):
    errors = []
    names = set(ctx.asmdefs)
    for name, (_, refs) in ctx.asmdefs.items():
        for r in refs:
            if name.startswith('TW.') and r.startswith('GUID:'):
                errors.append(
                    f'{name}: GUID reference {r} (use the TW.* assembly name — Unity wrote this by '
                    f'GUID, and sim_isolation, asmdef_cycles and test_modules cannot see it)')
            elif r.startswith('TW.') and r not in names:
                errors.append(f'{name}: unknown reference {r}')
    return errors
