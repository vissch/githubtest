"""The autoloop's 'rescue' step as a supervisor module: push a rebased lane that cannot fast-forward as <lane>-b, never forced (rescue).

The step's code is Tools/assetboard/autoloop.py's, unchanged: this file only calls it (Tools/supervisor/looprun.py
keeps the round's order, its one World a pass and dry's refusals). DEFAULT_MODE is 'off': the live autoloop runs the
step until this switch goes 'on' in modules.json, and from then the autoloop leaves it to the supervisor
(autoloop.supervised()). The switch-over order is in docs/reference/pipelines.md, the row of the loop_* modules.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import looprun  # noqa: E402

NAME = 'loop_rescue'
STEP = 'rescue'
ORDER = 6                 # its place in autoloop.STEPS; the file name's number says the same
EVERY_S = 30                # asked every pass; looprun runs the step only in a round's pass (every ROUND_S)
DEFAULT_MODE = 'off'


def run(ctx):
    return looprun.run_step(ctx, STEP)
