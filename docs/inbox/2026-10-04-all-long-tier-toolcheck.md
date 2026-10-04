# The run before a commit skips the Long tests, and the tools check themselves (2026-10-04)

From `lane/sim/long-tier` and `lane/show/verify-itself`. This note applies from the day they are on the integration
branch.

**The Long tier.** 47 sim and match tests that take over 3 s carry `[Test, Category("Long")]`. They are two thirds
of the EditMode run time. `gate.ps1 -EditOnly` (and `-Module`, `-EditOnly -All`) now leaves them out, so the run
before a commit takes about four minutes on a lane that touches the sim instead of eleven. `-EditOnly -Long` runs
them. The full gate is unchanged: every test, and still the only run `Tools/land.py` accepts.

What that costs you: most determinism and whole-battle tests are Long. A sim change that breaks one shows at the full
gate, not at the commit. After a sim change, run the full gate (or `-EditOnly -Long`) before you trust it.

**A new slow test.** Over 3 s: give it `[Test, Category("Long")]`. The gate prints how long EditMode took, and past
300 s it lists the slowest tests it ran.

**The tools.** `python Tools/toolcheck.py` is the one check of the docs and the tools: `validate.py`,
`Tools/selftest.py` and every `test_<tool>.py` under `Tools/`, no Unity, about two minutes. When your lane changes a
tool (`Tools/`, `validate.py`, `gate.ps1`, `.github/`):
- the gate runs it in place of `validate.py` alone;
- `land.py` runs it and refuses the lane while it is red, also when the lane changes code and its gate is green.

**When you rebase onto it:** nothing to move. A lane that edits `gate.ps1`, `Tools/land.py` or `Tools/selftest.py`
will conflict there; merge both sides by hand (`CLAUDE.md`, Integration), then `python Tools/toolcheck.py`.

`workflow.md`, sections 1 and 5. Delete this note from your lane once you have rebased.
