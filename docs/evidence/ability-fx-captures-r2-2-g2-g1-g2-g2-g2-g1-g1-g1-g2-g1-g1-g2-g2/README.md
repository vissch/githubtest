# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g2-g2: the plain mean does not close the seam either

Three fresh captures of the plain-mean build (`CombatFx.Banks.cs`, commit "gas bank: GasBankLoad is the plain
neighbourhood mean again"), with every bar in `probe.py` re-set from these three runs alone except
`pose_error_max` (unchanged). `probe.py` is the previous round's file with only its FRAME paragraph,
`BARS`/`BAR_VALUES` changed: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/` and no
MEASURE moved, so this round's numbers and the last round's compare directly.

## The complaint

> The same three game-camera stills (sim ticks 557, 637 and 731) show a dark seam through the bank.
> `row_gap_deepest` is 0.5853/0.6161/0.6330 and is unbarred. Close those seams so the deepest gap is at or under
> 0.40 on all three, and bar it. The bar has to sit outside the spread of three captures of the current build.

## The lever

`GasBankLoad` is the plain mean of the nine 4 m block maxima again, dropping the `max(own, mean)` fringe LIFT
added two levers back. The lift raised a fringe cell to its neighbourhood's thickness so the outermost ring would
draw thick enough to join the body; the previous two rounds showed the lifted ring is itself ragged, and its own
internal banding was the named suspect for the dark row. The plain mean trims that ring under the 0.8 stand floor
instead, so it does not draw at all. ONE lever only: the card pair, `GasBankZOffset`, `GasBankBandM`,
`GasBankWidth` and the stand floor all stay, so these captures compare straight to the tip's.

## It did not close the owner's key

`row_gap_deepest`, the key: **0.5890 / 0.6224 / 0.6540** against the previous round's 0.5853 / 0.6161 / 0.6330 —
unchanged, if anything a shade worse. The goal was 0.40 or under on all three. It is not met.

The mean-only round two levers back (`…-g2-g1`, before the card pair) read `row_gap_deepest`
0.2675 / 0.2502 / 0.2761, which is where the expectation of 0.24-0.31 came from. Removing the lift did **not**
bring that back, so the lift was not what the seam was made of: with the card pair in place the seam reads the
same whichever way `GasBankLoad` is computed. The pair halves the row pitch inside a cell and bridges the
patches, but the two stacked cards share one band profile, so the brightness down the frame still dips where
bands meet — and the pair's own stacked opacity (`1-(1-0.7*0.62)^2 = 0.68`) makes the bright core brighter
relative to the trough, which is what `row_gap_deepest` measures.

**The bar is not faked.** A bar that clears this build's own spread (0.0650) must sit at 0.6540 + 0.0650 =
**0.7190 or above**, far above the owner's 0.40, and the previous round's captures (max 0.6330) sit under that
value and would PASS it rather than fail. Neither half of the bar rule is met, so `row_gap_deepest` is reported
per run and stays **unbarred**, a fifth round running.

**The next lever.** Not a third `GasBankLoad` variant: the thickness rule has now been tried three ways
(own cell, neighbourhood mean, max-lift) and none of them moves this key. What is left is the pair's own
contrast — `GasBankPairAlphaK` (0.7) raised so the two cards of a pair sum flatter across the band, or lifting
the 0.8 stand floor so no ring draws near the paint threshold at all and the bank has no ragged edge rows to
band. Out of scope here, so that two levers are never stacked in one round.

## The bars

Every value from `probe.json`, t1 sim_tick 557 / 637 / 731. Rule unchanged: a bar must clear the nearest run by
at least the three runs' own spread (`le` bar = max + spread, `ge` bar = min − spread, rounded away from the runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_patch_count` | 17 / 17 / 15 | 2 | le 19 | 2 | 18 / 17 / 16 (bar was 20) |
| `bank_paint_share` | 0.9072 / 0.8683 / 0.8382 | 0.0690 | ge 0.76 | 0.0782 | 0.9094 / 0.8728 / 0.8325 (bar was 0.75) |
| `gas_paint_frac` | 0.0229 / 0.0213 / 0.0222 | 0.0016 | ge 0.0195 | 0.0018 | 0.0243 / 0.0201 / 0.0208 (bar was 0.0155) |
| `gas_green_share` | 0.5220 / 0.5124 / 0.6192 | 0.1068 | ge 0.40 | 0.1124 | 0.5227 / 0.5058 / 0.6647 (bar was 0.34) |
| `bank_low_share` | 0.0672 / 0.0724 / 0.0623 | 0.0101 | ge 0.052 | 0.0103 | 0.0651 / 0.0731 / 0.0768 (bar was 0.053) |
| `bank_height_frac` | 0.3360 / 0.3019 / 0.3458 | 0.0439 | le 0.40 | 0.0542 | 0.3540 / 0.3073 / 0.3203 (bar was 0.41) |
| `o240_area_frac` | 0.0253 / 0.0281 / 0.0305 | 0.0052 | ge 0.020 | 0.0053 | 0.0253 / 0.0280 / 0.0304 (bar 0.020) |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |

`row_gap_deepest` stays **unbarred** (see above). `row_dark_frac` stays unbarred too: runs
0.3834 / 0.3345 / 0.2941, spread 0.0893, so a bar that clears them sits at 0.4727 or above — looser than the 0.47
it was barred at two levers ago, and far from the goal of 0.08.

## What moved the wrong way

- **`row_gap_deepest`**, the owner's key: 0.5890-0.6540 against 0.5853-0.6330. Not closed, still unbarred.
- **`bank_low_share`** loosened 0.053 → 0.052: the trimmed fringe removes some of the lowest paint, so less of the
  bank sits in the bottom band.
- **`gas_paint_frac`** runs are slightly lower at the top end (0.0229 against 0.0243) — the lost fringe ring — but
  the spread tightened enough that the bar still rises, 0.0155 → 0.0195.
- **`bank_width_m`** (unbarred, printed in `probe.json`): 31.378 / 37.168 / 32.968, the same range as before; the
  trim does not change a single card's own width.

## What moved the right way

- **`bank_patch_count`** 17 / 17 / 15 against 18 / 17 / 16, bar 20 → **19**: the tightest this key has read in
  this lane. Trimming the near-threshold fringe takes away the cells that broke into their own small patches.
- **`gas_green_share`** bar 0.34 → **0.40**, **`bank_paint_share`** 0.75 → **0.76**, **`bank_height_frac`**
  0.41 → **0.40**, all tightened. Both isolation cards are still REJECTED, now by 2.90x and 2.74x the spread.
- **`gas_cards`** at T1 is **1478 / 1338 / 968**, back under `FlipbookFx.MaxCards` (1536) — the tip's run1 read
  1566, over the cap. The mean trims cells, as the plan predicted. `MaxCards` is untouched.

## The eye check

`run1/gas-t1.jpg` at the game camera: the bank reads as one wide green sheet with few separate blotches, and the
outermost ragged ring that the previous round's still showed at the bank's edge is gone — consistent with
`bank_patch_count` tightening and `gas_cards` falling. But a dark horizontal seam still crosses the bank's own
height, in the same place and about as deep as the previous round's, which is exactly what `row_gap_deepest`
reports. The fringe was not what made the seam.

## Checks

- `python probe.py` → exit 0, every bar OK, both isolation cards REJECTED. `row_gap_deepest` is NOT among the
  bars: the owner's goal for this round is not met, see above.
- `TW.Tests.BurstRecipeTests.TheGasBankLoadIsTheNeighbourhoodMean` → passes.
- `TW.Tests.BurstRecipeTests.TheGasBankRowsMeetAtHalfARowPitch` → passes (the pair is untouched).
- `TW.Tests.BurstRecipeTests.TheGasBankCardsOverlapIntoOneMass` → passes.
- `gate.ps1 -EditOnly` → exit 0.

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing. The owner's key this round,
`row_gap_deepest` at or under 0.40, is **not closed**, and the bar is left off rather than loosened to fit.

Out of scope here: the smoke bank, `GasBankBandM`, the pair itself, reshooting `cards/`, `tasks.md`'s probe
citation, turning the sheet into a puff, and landing the lane.
