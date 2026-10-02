WHY = 'Every C# file opens with a "// Phase:" line saying what it is for and which plan phase it belongs to.'


def run(ctx):
    errors = []
    for cs, txt in ctx.sources.items():
        first = txt.splitlines()[0] if txt.strip() else ''
        if not first.startswith('// Phase:'):
            errors.append(f'{cs}: missing "// Phase:" header')
    return errors
