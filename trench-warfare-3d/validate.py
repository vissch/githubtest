#!/usr/bin/env python3
"""Validation that runs without Unity: every check in Tools/checks/, in the order below.

    python validate.py                 from trench-warfare-3d/: run every check
    python validate.py --list          the checks and what each is for
    python validate.py --only a,b      only these checks (while fixing one; the gate always runs them all)

Prints `N assemblies, M C# files`, then either each error and exit 1, or `validation OK` and exit 0. The gate,
health.py, land.py and scorecard.py call it by this name and read that output, so both stay as they are.

A CHECK is one file, Tools/checks/<name>.py, with a one-line WHY and `run(ctx) -> list of error lines`. ctx (below)
reads the asmdefs and the C# sources once and hands them to every check. To add one: write the file, add its name to
ORDER, and give Tools/selftest.py a case that breaks the tree and sees the check catch it. A check that raises is
reported as one error line and the others still run.
"""
import importlib.util
import json
import pathlib
import sys

root = pathlib.Path(__file__).parent
CHECKS = root / 'Tools' / 'checks'

# The order the checks run and report in. A file in Tools/checks/ that is not listed here is an error, not a skip.
ORDER = ['asmdef_json', 'asmdef_refs', 'sim_isolation', 'asmdef_cycles', 'sim_purity', 'phase_header',
         'brace_balance', 'using_namespace', 'audio_volume', 'test_modules', 'codemap_docs']


def text(p):
    """Read a source file the way Unity writes one: UTF-8, BOM or not.

    pathlib's read_text() defaults to the locale encoding, which on Windows is cp1252. That decodes most of our files
    by luck and then dies on the first one that holds a character cp1252 has no slot for — a curly quote in a string
    literal was enough to take the whole validator down, and it took the whole team's gate with it."""
    return p.read_text(encoding='utf-8-sig')


class Context:
    """What the checks share. Files are read once here and passed around rather than re-read per check, which is
    also why this used to read some files four times over."""

    def __init__(self, root):
        self.root = root
        self.text = text
        self.warnings = []
        self._asmdefs = self._sources = None

    def asmdef_files(self):
        # Assets only: Library/PackageCache holds package asmdefs
        return list((self.root / 'Assets').rglob('*.asmdef'))

    @property
    def asmdefs(self):
        """name -> (path, references), for every asmdef that parses (asmdef_json reports the ones that do not)."""
        if self._asmdefs is None:
            self._asmdefs = {}
            for p in self.asmdef_files():
                try:
                    d = json.loads(text(p))
                except Exception:
                    continue
                self._asmdefs[d['name']] = (p, d['references'])
        return self._asmdefs

    @property
    def sources(self):
        """Every C# file under Assets/_Project: path -> text."""
        if self._sources is None:
            self._sources = {cs: text(cs) for cs in (self.root / 'Assets/_Project').rglob('*.cs')}
        return self._sources


def load(name):
    spec = importlib.util.spec_from_file_location('tw_check_' + name, CHECKS / (name + '.py'))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main(argv):
    on_disk = sorted(p.stem for p in CHECKS.glob('*.py') if not p.stem.startswith('_'))
    if '--list' in argv:
        for name in ORDER:
            print(f'{name:16} {load(name).WHY}')
        return 0
    names = ORDER
    if '--only' in argv:
        i = argv.index('--only')
        names = [n.strip() for n in (argv[i + 1] if i + 1 < len(argv) else '').split(',') if n.strip()]
        bad = [n for n in names if n not in ORDER]
        if bad or not names:
            print(f'validate: no check named {", ".join(bad) or "(none given)"}. The checks: {", ".join(ORDER)}')
            return 2
    ctx = Context(root)
    errors = []
    for name in sorted(set(on_disk) - set(ORDER)):
        errors.append(f'Tools/checks/{name}.py is not in ORDER in validate.py, so it never runs: list it there')
    for name in sorted(set(ORDER) - set(on_disk)):
        errors.append(f'ORDER in validate.py names the check {name}, and Tools/checks/{name}.py does not exist')
    for name in [n for n in names if n in on_disk]:
        try:
            errors += load(name).run(ctx)
        except Exception as e:   # one broken check must not hide what the others found
            errors.append(f'check {name} crashed ({type(e).__name__}: {e}): fix Tools/checks/{name}.py')
    for w in ctx.warnings:
        print(w)
    print(f'{len(ctx.asmdefs)} assemblies, {len(ctx.sources)} C# files')
    if errors:
        print('\n'.join(errors))
        return 1
    print('validation OK' + ('' if names is ORDER else f' (only {", ".join(names)})'))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
