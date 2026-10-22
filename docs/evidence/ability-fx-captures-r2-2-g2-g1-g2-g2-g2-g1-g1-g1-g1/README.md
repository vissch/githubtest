# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g1: close the row gap with a column stagger

**Part 1 of 2 (this commit).** The code fix, its two guard tests, and a sharpened `probe.py`. No captures yet:
`run1`/`run2`/`run3` are part 2, and `python probe.py` therefore still exits 1 with `three runs needed`.

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

## Part 2 (not this commit)

Three fresh captures of this build, every `BAR_VALUES` entry re-set from them (`row_dark_frac`'s bar must clear
them by their own spread AND stay under 0.1049 minus that spread, so the striped captures above fail it), and the
bar table. `bank_paint_share` / `bank_patch_count` are expected to move back towards 0.75 / 15-21 now that the
jitter is 1 m; report which way they went.

## This lane does not land

The puff brief `2026-10-21-gas-bank-turn-the-sheet-into-a-puff` (board id `4101179359`) is still unanswered.

## Out of scope

`DrawSmokeBank` and the smoke jitter, the gas hue and colour constants, reshooting `cards/`, turning the sheet
into a puff, and landing the lane.
