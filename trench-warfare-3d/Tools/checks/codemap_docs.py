import subprocess
import sys

WHY = 'The navigation docs match the tree: generated tables current, cited files real, every test, hook, tool and flag routed.'


def run(ctx):
    """CLAUDE.md and docs/reference/ against the tree. Tools/codemap.py explains each rule and how to fix it; its
    rules stay there, behind one check, because they share the parse of the docs."""
    errors = []
    cm = subprocess.run([sys.executable, str(ctx.root / 'Tools' / 'codemap.py'), '--check'], cwd=ctx.root, capture_output=True)
    cm_out = cm.stdout.decode('utf-8', 'replace').strip()
    for line in cm_out.split('\n'):
        if line.startswith('codemap: '):
            errors.append(line)
        elif line.startswith('warning: '):
            ctx.warnings.append(line)
    if cm.returncode != 0 and not any(l.startswith('codemap: ') for l in cm_out.split('\n')):
        errors.append('Tools/codemap.py --check failed: ' + (cm_out + cm.stderr.decode('utf-8', 'replace'))[-800:])
    return errors
