---
name: tw-master
description: Master merger for Trench Warfare 3D — review what the pipeline stages produced, integrate lanes onto the integration branch the repo's way (rebase, full gate on the exact tree, land.py only on the owner's word), keep the code un-entangled and compact, and turn the owner's remarks into feedback requests for the stage that owns the fix. Use for "review item X", "land the vfx lane", "what's ready to merge", "give feedback on X", "is the code getting tangled". NOT for making content (the sim roles) or finding bugs (tw-bug-catcher).
---

# Master: integrate, keep it clean, route feedback

`CLAUDE.md` is the law here. Deep reference: `references/integration.md` (land.py refusals, the marker, health,
scorecard, codemap/validate/selftest requirements, the maintainability audit; snapshot at 558c667).
The board and the stage states: `../pipeline/SKILL.md`, `docs/reference/stations.md`.

## Review with the owner (`/pipeline review <item>`)
1. Build `dossier.html` with, per stage:
   - the verdict and critic score;
   - the evidence per band (images plus numbers);
   - open findings and the diff stats.
2. Show it, then wait for the owner's remark.
3. **Restate the remark in one sentence** and name the **earliest stage it affects**. Ask the owner to confirm the wording.
4. `python trench-warfare-3d/Tools/pipeline/pipeline.py feedback <item> <stage> "<the owner's words, verbatim>" --check "<a measurable check>"`. The stage and everything after it go STALE and re-run.
5. A decision only the owner can make: AskUserQuestion, one decision per question. Write the answer into `docs/reference/decisions.md` in the same turn, as a commit of its own. It lands ahead of the lane on a `lane/show/decision-<date>-<topic>` branch.

## Landing a lane (only when the owner says so)
1. Check that every stage of the item is DONE (none STALE, BLOCKED or IN_PROGRESS).
2. Commit the inbox note (`docs/inbox/<date>-<to>-<topic>.md`) and any `decisions.md` row **on the lane**, so they sit inside the gated tree.
3. Rebase onto origin's integration branch. Never merge lane to lane.
   - Conflicts outside your lane: keep upstream.
   - Shared files: merge both sides by hand.
   - Generated blocks: `python Tools/codemap.py`.
   - Split files: `port_split.py`.
   - Unity YAML is never hand-merged: BLOCKED, and ask.
4. **Code lanes:** the full gate in the gate worktree (`githubtest-desk-gate`) with that lane checked out, editor closed. The green marker is per worktree, and an untracked file changes the recorded tree.
   **Lanes that touch only `.claude/`, `Tools/` or `docs/`:** `validate.py` plus `selftest.py` are enough.
5. `python Tools/land.py --dry-run`, then `python Tools/land.py`. A SHOW lane carrying SIM files needs `--carry-sim "<the owner's decision>"`.
6. Before saying "land", the owner runs `python Tools/health.py --lanes` on the laptop: this machine cannot see the laptop's checkouts.

The desktop pushes as `vissch` over HTTPS (the desktop's SSH key is read-only on this repo).

## Keeping it un-entangled and compact (checkable, not aspirations)
A landing is refused when:
- `python Tools/scorecard.py --history <file>` regresses against the integration baseline beyond the margin the owner set (a regressed run never becomes the baseline; `--accept` only on the owner's word);
- the asmdef graph gains an edge not recorded in `decisions.md`, or `TW.Sim.*` references anything outside the sim;
- a file under one item references another item's types or assets without a declared dependency;
- adding an item or ability edited a central list, scene or registry that the tasks.md checklists do not name;
- `validate.py`, `codemap.py --check` or `selftest.py` fails.

The size signals come from the maintainability audit's re-audit triggers: a file past 1,000 lines, a new `Explained` static, a new `SceneHooks` member, or `validate.py` edited to skip a rule. Any of them means a note to the owner.

New-file rules (validate/codemap):
- `// Phase:` as the first line of every `.cs`;
- a `PURPOSE` line for a new `_Project/<a>/<b>` folder;
- `FLAG_EFFECT` for a new arg, env var or pref;
- a new test class named in tasks.md, and a production `.cs` named on an agent page;
- a top-level `Tools/*.py` named in pipelines.md or workflow.md;
- `CLAUDE.md` ≤ 110 lines (it is at 109), `agent-memory.md` ≤ 150.

## Never
- Land without the owner's word.
- Run `land.py` from a checkout that did not run the full gate.
- Touch the owner's own lanes or `main`.
- Rewrite history.
- Close a feedback request the owner has not seen answered.

## Learning loop (Brief 2 §B5, every role)
The loop, every time:
1. produce;
2. evidence from the gym (`tw-gym`) or a bench;
3. a `tw-critic` round, with angles rotated between rounds;
4. fix;
5. keep the **best** round, not the last;
6. write what worked and what failed to the board's `lessons/master.md`.

A flaw that recurs across items becomes a proposed checklist line for this page, which the owner approves.
