# Patch author brief

You turn ONE card into ONE compiled patch, in your own scratch worktree. You do not run Unity and you do not touch
the main clone.

**Input.**
- The card's row, with its prediction.
- The worktree path, created from the orchestrator's HEAD.
- The files named in the card's evidence.
- The priors for the card's class from `priors.json`: hit rate, median gain, and calibration.
- `LESSONS.md`.

**Method.**
1. Read the code the card names. Grep for the declaration, not a guessed usage, and confirm the card's premise still
   holds. If it does not, stop and report `PREMISE FALSE` with the evidence. That is a valid and useful result.
2. Write the smallest change that could move the card's metric. If the constant is a knob (see `Knobs.cs` and the
   README "Knobs" section), prefer a SWEEP over a code change. The sweep needs no patch at all: report
   `SWEEP <knob> <v1,v2,v3>`.
3. Keep to the SHOW lane only, with no edits under `Sim/`, `Net/`, `Data/`, asmdefs or `ProjectSettings/`.
   - Every `.cs` starts with a `// Phase:` header.
   - Source is ASCII only.
   - Keep each file's line endings.
   - A new file gets a `.meta` with a fresh GUID.
4. Compile it: `python trench-warfare-3d/Tools/aosa/occ.py --changed` must print `occ OK` for every assembly. Also
   run `python trench-warfare-3d/validate.py`.
5. Choose the image class honestly:
   - **same image:** you can argue the pixels are equal, for example the maths is identical or it only skips
     invisible work.
   - **indistinguishable:** it changes pixels a little.
   - **juice:** it is meant to change the look.
6. Name the test that would catch a regression in this change. If none exists, write it in the same patch.

**Output: exactly this.**
```
CARD: <id>   RESULT: PATCH | SWEEP | PREMISE FALSE
PREDICTION: <metric> <delta> because <one line>   (revise it if your reading changed it)
IMAGE CLASS: same image | indistinguishable | juice
FILES: <list>
DIFF: <path to `git diff` saved as runs/<cycle>/<card>.patch>
OCC: <last lines>
TEST: <name, new or existing>
RISK: <what could break, which archetype/biome/tier to check>
```
