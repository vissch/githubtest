import re

WHY = 'Braces balance in every C# file: a cheap catch for a half-applied edit before an editor has to compile it.'


def run(ctx):
    errors = []
    for cs, txt in ctx.sources.items():
        t = re.sub(r'"(?:\\.|[^"\\])*"', '""', txt)
        t = re.sub(r'//.*', '', t)
        if t.count('{') != t.count('}'):
            errors.append(f'{cs}: unbalanced braces')
    return errors
