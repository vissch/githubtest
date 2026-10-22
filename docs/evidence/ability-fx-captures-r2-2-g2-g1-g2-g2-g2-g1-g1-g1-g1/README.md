# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g1: close the row gap with a column stagger

**Part 1** (commit `657901752b`): the code fix, its two guard tests, and a sharpened `probe.py`. **Part 2**
(this commit): the three captures of that build. They show the stagger did not close the row gap on screen
(see below), so no bar is set and `python probe.py` still exits 1 — now with `row_dark_frac ... FAILED`
rather than `three runs needed`.

## The owner's complaint

> At zoom 30 the three gas-t1 stills are still horizontal yellow-green bands with dark ground between the rows.
> `row_dark_frac` reads 0.014, 0.014 and 0, so the 0.03 bar passes a picture that is still striped. Close the gaps
> until one bank reads, and put the dark-row bar outside three captures of this striped build.

## Why no band height can close it (the arithmetic)

T1 pose: zoom 30, pitch 25 (`gas-t1.json`). sin 25 = 0.4226, cos 25 = 0.9063, tan 25 = 0.4663.

| | |
|---|---|
| one field row down the screen | `4 * sin 25` = **1.690 m** |
| a card's visible band | `GasBankBandM` 1.78 less the 0.08 m sink = 1.70 m |
| what that band covers on screen | `1.70 * cos 25` = **1.541 m** — short by 0.149 m, every row |
| the most the head rule allows (`band - sink < 1.8`) | `1.8 * cos 25` = 1.631 m — still under 1.690 |

So a taller band is a dead end, and `GasBankThin` 0.28 was already tried and made both row measures worse
(previous round's README). **The row pitch has to halve.** Last round's full-cell random jitter only randomised
the gap: over a 32x32 grid the worst spacing between two neighbouring columns' merged depths is still 5.95 m.

## The fix

`CombatFx.Banks.cs`: alternate columns stand half a field cell back in depth (`GasBankStaggerM` 2 m) and the
jitter drops to a quarter cell (`GasBankJitterM` 4 m -> 1 m). A card is 13.5-19.3 m wide, so three to four columns
overlap at any screen x and the two columns' depths interleave. Worst merged spacing, measured over a 32x32 grid
with the shipped hash: **2.99 m** = 1.268 m of screen, inside the card's 1.541 m reach (margin 1.22x). No cards
are added (775 today, `MaxCards` 1536), and `DrawSmokeBank` passes `0f` — smoke is untouched.

## The guard tests (`BurstRecipeTests`)

- `TheGasBankCardsLeaveNoGapDeeperThanACardAtTheT1Pose` (replaces `TheGasBankCardsFillTheDepthEvenly`, which only
  checked bin evenness): merges each column pair's depths and asserts every consecutive spacing is within a card's
  depth reach `(GasBankBandM - 0.08) / tan 25` = 3.646 m. Worst spacing now 2.99 m; at the old 1.2 m jitter it is
  4.58 m and at last round's 4 m random jitter 5.95 m, so both old builds fail it.
- `TheGasBankBandsMeetInDepthAtTheT1Pose`: the old 0.90-of-a-row factor is what let the stripe through (and it
  ignored the sink). It now asserts `(band - 0.08) * cos 25 >= GasBankStaggerM * sin 25` (1.541 >= 0.845) at all
  four (thick, hash) corners.

## The sharpened measure: `ROW_TROUGH_FLOOR` 0.75 -> 0.85

At 0.75 the striped build read `row_dark_frac` 0.0140 / 0.0140 / 0.0000. A `le` bar must sit outside a build's own
spread, and no number below 0 exists, so at that floor the key could never be barred outside the striped build —
exactly the hole the owner found. The striped build's deepest core rows sit at 0.70-0.76 of its own median
(`row_gap_deepest` 0.2438-0.3012), so 0.85 brackets them.

Re-measured with the sharpened floor
(`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1`, exit 1 as always):

```
run1   row_dark_frac 0.1399   row_gap_deepest 0.2980   t1 sim_tick 556
run2   row_dark_frac 0.1049   row_gap_deepest 0.3012   t1 sim_tick 596
run3   row_dark_frac 0.1223   row_gap_deepest 0.2438   t1 sim_tick 636
```

All three are well over the 0.04 the plan asked for, so the striped build now reads as striped and a bar outside
it is possible. Nothing else in the probe moved; `CARDS_DIR` still points at
`../ability-fx-captures-r2-2-g2-g1/cards/` and the cards are not reshot.

## Part 2: the captures say the stagger did not close the gap

Three fresh captures of the committed stagger build (`657901752b`, t1 `sim_tick` 557/597/637):

```
run1   row_dark_frac 0.1644   row_gap_deepest 0.2671
run2   row_dark_frac 0.1644   row_gap_deepest 0.2594
run3   row_dark_frac 0.1517   row_gap_deepest 0.2636
```

This is HIGHER than the striped build it was meant to beat (0.1399 / 0.1049 / 0.1223, `../ability-fx-captures-r2-2
-g2-g1-g2-g2-g2-g1-g1-g1`). The arithmetic in "Why no band height can close it" above shows the worst merged
column-pair spacing shrinks to 2.99 m, inside a card's 3.646 m reach — but `row_dark_frac` reads the on-screen
row profile, not column-pair spacing, and on screen the stagger reads as MORE dark row than before, not less.
No `le` bar exists that clears this build's own spread (0.0127) and also sits under the striped build's floor
minus its spread (0.1049 - 0.0350 = 0.0699): the window the plan asked for is empty, in the wrong direction (this
build is worse, not merely "near"). Per the plan's own risk note, the bar is not raised to fit; `row_dark_frac`
stays at 0.03 and this round's `probe.py` is left at `three runs needed` / unset — see `note.md` in this round's
leg folder.

Every other key clears easily on the new runs: `gas_paint_frac` 0.0215/0.0208/0.0195, `bank_paint_share`
0.6711/0.6811/0.7174, `bank_patch_count` 26/24/19, `gas_green_share` 0.4270/0.4187/0.4222, `bank_low_share`
0.0994/0.1009/0.1055, `bank_height_frac` 0.2459/0.2288/0.2249, `o240_area_frac` 0.0169/0.0192/0.0196,
`pose_error_max` 0.0000 all three. Only `row_dark_frac` (and the unbarred `row_gap_deepest`) fail to clear.

`run1`/`run2`/`run3` in this folder are these three captures, packed and committed so the next round does not
reshoot.

## This lane does not land

The puff brief `2026-10-21-gas-bank-turn-the-sheet-into-a-puff` (board id `4101179359`) is still unanswered.

## Out of scope

`DrawSmokeBank` and the smoke jitter, the gas hue and colour constants, reshooting `cards/`, turning the sheet
into a puff, and landing the lane.
