import json

WHY = 'Every asmdef and Packages/manifest.json is valid JSON: Unity drops an assembly it cannot parse without saying which.'


def run(ctx):
    errors = []
    for p in ctx.asmdef_files() + [ctx.root / 'Packages/manifest.json']:
        try:
            json.loads(ctx.text(p))
        except Exception as e:
            errors.append(f'invalid JSON {p}: {e}')
    return errors
