# Navigation eval: can a fresh agent find the right files?

A test of these docs, for whoever maintains them. Fresh agents get a change request as a user would phrase it and
only the repo, and are scored on whether they name the files the real change touched. The answers come from real
commits, so this measures real work. It is not an agent page: nothing in `CLAUDE.md` sends agents here.

## Method
1. **Pick tasks the docs have never seen.** Commits made after the docs snapshot you are testing, on any lane,
   touching one to five existing code files. Write each as the request a user would make, without file names. Never
   fix the docs for a task and then count that task again: a doc fixed for a task passes it.
2. **Keep the answer key out of the repo.** Note only the commit subjects here. The grader gets the files with
   `git show --name-only <commit>` at grading time.
3. **Isolate the agents.** A detached worktree of the snapshot, no git commands, told to stay in that folder. The
   answers exist on other branches and in sibling worktrees, so isolation is by instruction only; say so in the result.
4. **Run a control.** The same tasks, same model, with `CLAUDE.md` and `docs/reference/` deleted from the worktree.
   The difference is what the docs are worth. Without a control a score says little.
5. **Grade blind and on more than the first file.** A second agent, which did not write the docs, scores each answer
   against the commit: file recall and precision, whether the tests named would have run, whether the lane and seam
   rules were respected. Record tool calls, tokens and time per task, and report the spread, not only a count.

Prompt (fill in the checkout path and four tasks):
```
You are a fresh coding agent dropped into an unfamiliar Unity game repository, asked to plan four changes. Only
navigate: for each task find the file(s) you would edit, the test(s) you would run, and how you would verify. Do not
edit. Checkout: <path> (the Unity project is trench-warfare-3d/). Start the way the repo tells a new agent to start
(CLAUDE.md) and use its docs as intended; search code when you naturally would. No git commands, no other folders.
Tasks: <four tasks>
For EACH task: edit (paths, most important first) / tests / see it / found via (doc + section, or search) /
steps (tool calls) / confidence / friction. End with DOC PROBLEMS and your total tool calls.
```

## Tasks used so far (do not reuse: the docs were fixed for every one)
walker body on the ground (cb68b59) · flamethrower pools on the sim clock (1cf6e56) · HUD bar width counts support
cards (482de9f) · one aiming radius (34c72d1) · debug panel arms only fielded abilities (cfdc534) · shadow distance
knob (f8dd628) · cheaper crater hollow rescan (55d15e0) · hide combat overlays in bench stills (7193323) · bench picks
its battlefield (ed26a80) · launch request names faction and units (7847b5f) · crater colour upload cadence (aec190d) ·
man lit before the burning system steps (0a9af6a) · picker projects with one matrix (5378697) · charcoal night smoke
(dd6bef2) · thinner barrage smoke at soldier height (763664a) · stress preset spreads the army (e6bd10c) · every draw
through FrameBudget (5932df5) · wreck provenance (a176a01) · hide the debug panel button in bench stills (dd9e3b9) ·
repeatable player stills (2c4f3f3) · pure banner text (e68788f) · damaged trench lining (ff9e200) · bench warns on
unknown options (7730ee3) · one banner per event (9b7a09d) · a dead man's fire goes out (d3c0936) · bench holds the
camera on x/z (72cc25b) · advance-order archetype mask (76b0542) · explosion source ids (1acc943) · one unit-look
table and ten deploy keys (7c54acf) · torch on the sim clock (2f1ff57) · aim preview through a hook (6582cef) · walker
as a field (fba5e18).

## Results
| Date | Docs at | Tasks | First file right | Right file in first three | Tool calls a task | Limits |
|---|---|---|---|---|---|---|
| 2026-09-27 | e7de2bf | 12 | 11 of 12 | 11 of 12 | about 7 | no control, graded by the docs' author, first file only |
| 2026-09-27 | e7292bd | 8 held out | 6 of 8 | 8 of 8 | about 7 | same, and only 8 tasks |
| 2026-09-27 | 227a009 | 6 held out, from lanes not yet landed | 5 of 6 | 6 of 6 | about 5 | no control, graded by the docs' author; tests right 2 of 4 |
| 2026-09-27 | 92f6a29 | 6 held out (3 SIM, 3 SHOW) | 6 of 6 | 6 of 6 | about 5 | same; file recall low on the HUD table (4 of 8 files) and the walker change (6 of 10) |

What the two rounds found (all fixed since): no rows for support-fire aiming, sim fire, render settings, factions,
the stress preset or objectives; a flamethrower row that said the sim has no fire; bench options documented that
exist only on another lane; a code comment claiming every draw goes through `FrameBudget`; three SHOW files listed
under the SIM heading. A 2026-09-27 critique then found 57 production files named on no agent page, now enforced by
`validate.py`. The third round (227a009) found: the centre banner routed only through the objectives row, the HUD
test names swapped in the agent's head (HudTextTests checks tooltips, HudBindTests checks `HudText`), no mechanism in
the debris row, and a flamethrower row that said a burning man runs on sim ticks while his torch does not. The
fourth (92f6a29) found no row for explosion source ids, the advance-order mask limit, the unit checklist's missing
keys and portrait copies, and the walker call sites. The next run should follow the method above in full; until
then these numbers flatter the docs.
