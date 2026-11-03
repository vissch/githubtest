"""The autoloop's 'attempts' step as a supervisor module: park a unit that went nowhere in its tries and put it to the owner once (attempts).

The step's code is Tools/assetboard/autoloop.py's, unchanged: this file only calls it (Tools/supervisor/looprun.py
keeps the round's order, its one World a pass and dry's refusals). DEFAULT_MODE is 'off': the live autoloop runs the
step until this switch goes 'on' in modules.json, and from then the autoloop leaves it to the supervisor
(autoloop.supervised()). The switch-over order is in Tools/supervisor/README.md.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import looprun  # noqa: E402

NAME = 'loop_attempts'
STEP = 'attempts'
ORDER = 3                 # its place in autoloop.STEPS; the file name's number says the same
EVERY_S = 30                # asked every pass; looprun runs the step only in a round's pass (every ROUND_S)
DEFAULT_MODE = 'off'


def run(ctx):
    return looprun.run_step(ctx, STEP)
