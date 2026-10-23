# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g2: the overview as one cloud, the sheet back to full width

**Three runs are shot and every bar is re-set from them. `python probe.py` exits 1, on one key only:
`gas_green_share`.** All three runs sit above the owner's 0.44 (0.4828 / 0.4431 / 0.4478) but the nearest clears
it by 0.0031 against the runs' own spread of 0.0397, and the owner's rule is that the bar must sit OUTSIDE the
spread of three captures. The plan's named fallback was shot and measured and does not close it (below). The bar
is left at the owner's 0.44 and reads FAILED; it was not lowered. Everything else he asked for is in: the
overview is one cloud, the sheet is back to full width, and `bank_patch_count` is far inside his cap.

`probe.py` is the previous round's file with only its FRAME paragraph changed, `measure_o240` extended and
`BAR_VALUES` re-set. No MEASURE already in it moved and `CARDS_DIR` still points at
`../ability-fx-captures-r2-2-g2-g1/cards/`, so every existing key compares straight to the previous round's
`probe.json`.

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

The baseline the reshoot had to beat, measured with THIS probe on the previous round's captures
(`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1`, exit 1 by design):

| run (t1 sim_tick) | `o240_dark_frac` | `o240_gap_deepest` | `o240_band_lift_med` |
|---|---|---|---|
| run1 (557) | 0.1415 | 0.4944 | 4.922 |
| run2 (597) | 0.1204 | 0.4888 | 4.333 |
| run3 (637) | 0.1122 | 0.4007 | 5.532 |

For reference, the same striped runs' T1 row keys are `row_dark_frac` 0.0331/0.0733/0.0608 and `row_gap_deepest`
0.2807/0.2911/0.2830 — the T1 frame was already much less striped than the overview, which is exactly why the
complaint is about the overview and why the T1-only keys could not see it.

## The three runs

`TW_GAS_EXTRA_FRAMES` 0 / 30 / 60, each a gas pass and a plate pass of `GasBankStills`, on commit `ac94229cee`'s
build. t1 sim_tick 557 / 597 / 637, o240 sim_tick 249 / 269 / 289, `pose_error_max` 0.0000 on all four stills of
all three runs.

| key | run1 | run2 | run3 | previous round (striped) | reading |
|---|---|---|---|---|---|
| `gas_green_share` | 0.4828 | 0.4431 | 0.4478 | 0.4357 / 0.4102 / 0.4193 | all three above the owner's 0.44, but the spread is 0.0397 — see "The green share" |
| `o240_dark_frac` | 0.0093 | 0.0000 | 0.0000 | 0.1415 / 0.1204 / 0.1122 | **the complaint, closed**: the overview stripes are gone |
| `o240_gap_deepest` | 0.2574 | 0.1705 | 0.1463 | 0.4944 / 0.4888 / 0.4007 | the deepest overview trough is a third to a half as deep |
| `o240_band_lift_med` | 9.782 | 9.174 | 10.468 | 4.922 / 4.333 / 5.532 | the overview bank is about twice as bright: merged cards paint their whole 8 m block |
| `bank_width_m` | 31.248 | 35.778 | 35.911 | 36.40 / 24.16 / 28.72 | **the skirt is back**: the narrowest run is above two of the three previous ones, and runs 2 and 3 above all of them |
| `bank_patch_count` | 15 | 14 | 5 | 19 / 13 / 6 | far inside the owner's cap of 38; the wider cards did NOT re-open the islands |
| `bank_paint_share` | 0.8750 | 0.8574 | 0.9296 | 0.8431 / 0.8497 / 0.9188 | one sheet, slightly more of it in one patch than before |
| `gas_paint_frac` | 0.0205 | 0.0189 | 0.0162 | 0.0195 / 0.0183 / 0.0161 | back to the previous round's level after the stand floor halved it |
| `o240_area_frac` | 0.0309 | 0.0337 | 0.0343 | 0.0194 / 0.0215 / 0.0220 | the overview bank covers half again as much frame |
| `row_dark_frac` | 0.0000 | 0.0000 | 0.0000 | 0.0331 / 0.0733 / 0.0608 | no dark mud line at T1 at all |
| `row_gap_deepest` | 0.1998 | 0.2377 | 0.2264 | 0.2807 / 0.2911 / 0.2830 | reported only (barring it is out of scope) |
| `bank_low_share` | 0.0611 | 0.0643 | 0.0715 | 0.0738 / 0.0818 / 0.0804 | **moved the wrong way**: less of the bank sits under a man's head, because the cards reach higher |
| `bank_height_frac` | 0.3614 | 0.3082 | 0.2996 | 0.3007 / 0.2747 / 0.2716 | **moved the wrong way**: the bank stands higher in frame; run1 is above the previous round's bar of 0.33 |
| `bank_rise_m` | 7.955 | 7.379 | 7.167 | 6.52 / 6.90 / 7.22 | the same fact in metres: wider cards reach further up the screen |

The two keys that moved the wrong way are the price of the wider cards the owner asked for, and both bars are
re-set from these runs rather than kept: `bank_low_share` 0.065 -> 0.05, `bank_height_frac` 0.33 -> 0.43.

## The bars

Every bar is set from these three runs alone, by the standing rule: a `ge` bar at or under min - spread, a `le`
bar at or over max + spread, rounded away from the runs.

| key | cmp | bar | previous round's bar | margin / spread |
|---|---|---|---|---|
| `bank_low_share` | ge | **0.05** | 0.065 | 0.0111 / 0.0104 |
| `bank_height_frac` | le | **0.43** | 0.33 | 0.0686 / 0.0618 |
| `o240_area_frac` | ge | **0.027** | 0.016 | 0.0039 / 0.0034 |
| `pose_error_max` | lt | 0.5 | 0.5 | 0.5000 / 0.0000 |
| `gas_paint_frac` | ge | **0.011** | 0.012 | 0.0052 / 0.0043 |
| `gas_green_share` | ge | **0.44** (the owner's) | 0.38 | 0.0031 / 0.0397 — **FAILED** |
| `bank_paint_share` | ge | **0.78** | 0.76 | 0.0774 / 0.0722 |
| `bank_patch_count` | le | **25** | 32 | 10 / 10 |
| `o240_dark_frac` | le | **0.02** (new) | — | 0.0107 / 0.0093 |
| `row_dark_frac` | le | **0.02** | 0.12 | 0.0200 / 0.0000 |

`o240_dark_frac` is barred for the first time, at the band the complaint is about: 0.02 against the striped
build's 0.1415 / 0.1204 / 0.1122 on this same probe, five times the bar, so the key tells the rejected build from
this one.

`bank_width_m` is **not** barred. The sheet is as wide as the owner asked (31.248 / 35.778 / 35.911 m against the
previous round's narrowest of 31 m), but the spread is 4.663 m, so the rule allows only a bar at or under
26.585 m — under the 31 m a width bar would have to carry to mean anything. Reported, not barred.

Both isolation cards are still REJECTED by a wide margin: `cards-smoke` 0.0908 and `cards-puff` 0.1076, under the
0.44 bar by 8.8x and 8.4x the runs' spread.

## The green share: why this probe exits 1

The owner's floor is 0.44 and the bar is at 0.44. The three runs read 0.4828 / 0.4431 / 0.4478, so the build does
clear his number on every capture — but the nearest run clears it by 0.0031 while the three runs' own spread is
0.0397, and his rule is that "the bar has to sit outside the spread of three captures of the current build". By
that rule the bar this build earns is 0.4034, and lowering the bar to it is exactly what he refused.

The plan's named fallback for this case was tried, not guessed: `GasBankThin` 0.19 -> 0.26, one calibration
capture at the `TW_GAS_EXTRA_FRAMES` 30 pose (the run that sets the minimum). It moved that pose from
**0.4431 to 0.4535**, about a tenth of the 0.037 the margin needs, while `bank_patch_count` held at 13. Raising
the thin alpha far enough to close the gap would also break two standing guards in `BurstRecipeTests`
(`TheGasBankStacksIntoDensity`'s "a thin cell's card is a haze, not a tile", `GasBankAlpha(0) <= 0.2`, and the
0.319 hand value in `TheGasBankStandFloorKeepsEveryCardOverHalfTheCore`), which is a look decision, not a number
to turn. So the build was left at 0.19, the measurement is recorded here, and the bar stays at the owner's 0.44.

What would close it is narrowing the SPREAD, not raising the mean: run1 (the earliest tick) is the outlier at
0.4828 and the two later ticks sit together at 0.443-0.448. That is a question about which three poses the round
is shot at, and it is the owner's to answer.

## The eye check at O240

`run1/gas-o240.jpg`, `run2/gas-o240.jpg`, `run3/gas-o240.jpg` (sim ticks 249 / 269 / 289): one continuous green
sheet over the trench line, no dark mud rows across it, no rectangle edge. The striped rectangle of the previous
round is gone, which is what `o240_dark_frac` 0.0093 / 0.0000 / 0.0000 says in numbers.

## The checks

- `python probe.py` -> **exit 1**, `gas_green_share` FAILED on margin, every other bar OK with margin >= spread,
  both isolation cards REJECTED.
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1` -> exit 1 by
  design, printing the striped build's baseline quoted above.
- No production file changed in this commit: the `GasBankThin` 0.26 experiment was reverted after it was
  measured, so the three runs and the committed build are the same thing.

## Where this stands

**Blocked on the owner.** The build does everything he asked except give `gas_green_share` 0.44 a margin wider
than the three captures' spread, and he barred lowering that bar. The two ways forward are his to pick: accept
0.44 as a floor the runs clear without the spread rule, or change the three capture poses so the spread is not
0.0397 wide.

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing.

Out of scope here: lowering `GasBankStandFloor`, `GasBankLoad`'s formula, the gas hue and its colour constants,
the smoke bank, reshooting `cards/`, barring `row_gap_deepest`, `tasks.md`'s probe citation, turning the sheet
into a puff, and landing the lane.
