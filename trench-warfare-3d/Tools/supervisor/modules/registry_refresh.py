"""Keep the registry's cache (Tools/supervisor/registry.py) fresh: done records of the boards, unit tags on the
origin lanes and on integration. Writes only its own state; what reads it acts only when this switch is 'on'
(supervisor hooks: the relay's picker and the loop's feed pass a done unit by)."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import registry  # noqa: E402

NAME = 'registry_refresh'
EVERY_S = 600
DEFAULT_MODE = 'dry'


def run(ctx):
    st, lines = registry.refresh(ctx.state())
    ctx.save(st)
    return lines
