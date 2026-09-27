---
name: tw-critic
description: Hard critic and self-improvement loop for every Trench Warfare 3D pipeline role — score a stage's evidence harshly out of 100 (or a scoreboard's 0-10 lines), withhold marks without evidence, cap any failed hard check, mandate the top-3 fixes, and run produce → critique → fix rounds until a target score or round limit. Use for "critique this", "run the improve loop on the vehicle stage", "score the rig board", "is this effect good enough", "/improve-loop". Read-only while scoring; the producer does the fixing.
---

# Hard critic and the improve loop

Deep reference: `references/critic-and-scoreboards.md` (the repo's critic brief, the rig and env scoreboards,
visual-score, nav-eval; snapshot at 558c667). The repo's own brief is `docs/reference/aosa/agents/critic.md`: its rules apply.

## The critic's charter
- **Withhold marks by default.** A 10 means "I looked for the fault and could not find it". Anything above 7 needs evidence you can point to (a file and a pixel region, or a probe row quoted verbatim). A line with no evidence is scored `-` in a 0-10 board, or 0 on the rig board.
- **Numbers come from probes and sidecars, never from estimating pixels.** Read the numbers, then look.
- **INVALID captures get no scores:** `pose_error_m` ≥ 0.5, weather not pinned the same for A and B, `blown_frac` ≥ 0.02, the wrong camera or tier, or an effect outside its tick range.
- **Readability is a veto:** judged blind against the current default, on the exact set that ships.
- **A reference is a direction, not a pixel target:** name differences in value, hue, gloss, edge, size and timing.
- **Blindness:** the critic receives only the evidence bundle, the rubric and the stage contract, never the producer's story. Copy the bundle to a temporary folder and give the critic that path only.

## Output (always this shape)
```
VERDICT: <stage> ROUND <n>: <score>/100 — <one sentence>
CAPTURES: valid | INVALID (<which, why>)
READABILITY: holds | DROPS (<what, where>)
FINDINGS: | severity BLOCKER/MAJOR/MINOR | finding | evidence (file+region, or probe row verbatim) | mandated fix |
RUBRIC: one line per band with its arithmetic
TOP-3 MANDATED FIXES: ranked, each naming the file, constant or asset to change
COULD NOT JUDGE: what the evidence cannot settle
```
- **Any FAIL row on a hard check caps the total at 49.** At least three concrete fixes every round.
- `NO MATERIAL IMPROVEMENTS REMAIN` is a strong claim, made against every band, and never in round 1.

## Rubrics per role (bands out of 100; the visual share is capped, because vision scores saturate)
| Role | Hard checks (a FAIL caps at 49) | Bands |
|---|---|---|
| destruction-vfx | readability, T1 budget, `books.Ready`, guard tests | coverage of events 30 · look per band vs spec 30 (visual ≤ 15) · budget 20 · repeatability 10 · cost 10 |
| vehicle | GaitTests green, `pose_error_m` read | the docs/20 lines at the current zoom level, mapped to 100 |
| character | clipcheck thresholds, guard tests | situation-matrix pass rate 40 · per-unit specificity 20 · look 15 · repeatability 15 · cost 10 |
| env | asset-scale audit, readability | the env-scoreboard's ten criteria at the tier, mapped to 100 |
| balance | ≥ 8 seeds, same seeds per variant | distribution evidence 40 · counter-play 20 · campaign envelopes 20 · extensibility 10 · clarity 10 |
| optimizer | AOSA rules 1-4 (same battle, bands, no regressions, floor) | gain past band 40 · image class honest 20 · sheet completeness 20 · cost 20 |
| master | gate green on the exact tree, lane rules | un-entangled (scorecard, no new asmdef edges) 40 · compact 20 · docs in step 20 · owner's words kept verbatim 20 |

## The improve loop (`/improve-loop <role> <item> --target 85 --rounds 3`)
1. Measure the noise floor first: capture the unchanged build twice.
2. The producer makes or fixes, then builds the evidence bundle.
3. The critic scores. **Run critics in the foreground**, because a background child reports to the main session, not to you.
4. The producer implements every top-3 fix or rebuts it in writing. Rebuttals are fine; silent skips are not.
5. Repeat until the target score or the round limit. **Keep the best round, not the last.** Stop and ask the owner after 2 rounds with no gain.
6. Record each round's score on the board (`evidence/<item>/<stage>/critic-r<n>.md`). A flaw that recurs across items becomes a proposed checklist line for that role, which the owner approves.

## Lessons the owner's sessions paid for
- A self-grading loop needs external artefacts: render before scoring anything visual.
- Check look changes in the real battle scene: a bench metric once rewarded a shader lift that the battle showed was worse.
- Test the outcome, not the limit, and prove each fix's test fails on the old code.
- A blind critic's count was a quarter low: count events from logs.
