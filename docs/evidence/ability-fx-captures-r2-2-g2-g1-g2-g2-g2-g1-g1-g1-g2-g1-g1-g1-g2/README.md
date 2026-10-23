# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g2: the overview as one cloud, the sheet back to full width

Work in progress. `probe.py` is here and the new overview key is calibrated on the previous round's captures; the
three fresh runs of this build are not shot yet. Until `run1/`-`run3/` exist, `python probe.py` exits 1 with
"three runs needed".

`probe.py` is the previous round's file with only its FRAME paragraph changed and `measure_o240` extended. No
MEASURE already in it moved and `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/`, so every
existing key compares straight to the previous round's `probe.json`. `BAR_VALUES` is still the previous round's
and will be re-set from this build's own three runs.

## The complaint

> This pass lowered `gas_green_share` from the owner's 0.44 to 0.38, and the runs are 0.4357/0.4102/0.4193. At the
> overview the three stills (sim ticks 249, 269, 289) are one striped rectangle, and runs 2 and 3 lost 5 to 9 m of
> width because the higher floor deleted the skirt. Put the floor back to 0.44, keep the sheet as wide as the
> previous round, and reshoot so the overview is one cloud. The bar has to sit outside the spread of three
> captures of the current build.

"The floor" here is the **bar** `gas_green_share` 0.44, not `GasBankStandFloor`: the 2.4 m stand floor is what
bought `bank_patch_count` 19/13/6 and lowering it is out of scope.

## The levers (commit `dd2ad63fdb`, already on this lane)

- **`GasBankSquashFor` and `GasBankZOffset` scale with the merge step.** `BankStep` doubles a merged card's WIDTH
  but its painted depth stayed put: the band held at 1.78 m and the depth spread at +-2 m against an 8 m row
  pitch, so half of every merged row was bare mud — the striped rectangle the owner saw in the three overview
  stills. The band is now 1.78 m at step 1 and **3.56 m at step 2**; the depth spread +-2 m then **+-4 m**, the
  whole block. Step 1 is bit-for-bit unchanged, so no T1 bar can move on it.
- **`GasBankWidth` 18 + 4 * thick -> 26 + 4 * thick** (23.4-33.0 m against 16.2-24.2 m) puts back the 5-9 m of
  width the 2.4 m stand floor took off the bank's outer skirt, and thickens the paint by overlapping six to eight
  deep instead of four to five. Card height does not move: `width * GasBankSquashFor(width, step)` cancels the
  width. The floor, the alpha and the hue are untouched.

## The new key: the overview gets a row profile

Until now every row key was read at T1 only, while the striping the owner complained about is at the OVERVIEW.
`measure_o240` now runs the same `row_profile_stats` on the O240 pair and re-keys it as **`o240_dark_frac`**,
**`o240_gap_deepest`** and **`o240_band_lift_med`**.

The key discriminates: on the striped build it reads well clear of 0, so a bar can be set on it once this build's
three runs are in. The baseline the reshoot must beat, measured with THIS probe on the previous round's captures
(`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1`, exit 1 by design):

| run (t1 sim_tick) | `o240_dark_frac` | `o240_gap_deepest` | `o240_band_lift_med` |
|---|---|---|---|
| run1 (557) | 0.1415 | 0.4944 | 4.922 |
| run2 (597) | 0.1204 | 0.4888 | 4.333 |
| run3 (637) | 0.1122 | 0.4007 | 5.532 |

Worst 0.1415 and 0.4944, spread 0.0293 and 0.0937. So a `le` bar on `o240_dark_frac` only carries if this build
reads under 0.1415 minus its own spread; the same for `o240_gap_deepest` under 0.4007. Those two lines are what
"the overview is one cloud, not a striped rectangle" means in numbers, and whether either can be barred is
decided from this build's own three runs, not from these.

For reference, the same striped runs' T1 row keys are `row_dark_frac` 0.0331/0.0733/0.0608 and `row_gap_deepest`
0.2807/0.2911/0.2830 — the T1 frame was already much less striped than the overview, which is exactly why the
complaint is about the overview and why the T1-only keys could not see it.

## Still to do

1. Three fresh captures of this build into `run1/`-`run3/`.
2. Re-set every bar from those three runs, with `gas_green_share` at **0.44 or higher** and `bank_patch_count` at
   or under the owner's cap of **38**. The named fallback if the green share falls short is `GasBankThin`
   0.19 -> 0.26 (the thinnest standing card then draws 0.26 + 0.36 * 0.3 = 0.368, still past the half-core 0.31
   that `TheGasBankStandFloorKeepsEveryCardOverHalfTheCore` guards). The bar is never lowered below 0.44.
3. `python probe.py` -> exit 0.

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing.

Out of scope here: lowering `GasBankStandFloor`, `GasBankLoad`'s formula, the gas hue and its colour constants,
the smoke bank, reshooting `cards/`, barring `row_gap_deepest`, `tasks.md`'s probe citation, turning the sheet
into a puff, and landing the lane.
