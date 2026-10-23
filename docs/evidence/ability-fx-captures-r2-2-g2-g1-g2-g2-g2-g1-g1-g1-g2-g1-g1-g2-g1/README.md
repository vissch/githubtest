# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g2-g1: the plain mean, reshot, and the owner's key still does not close

Three fresh captures of the plain-mean build (`e373c114df`: `GasBankLoad` back to the plain mean of the nine 4 m
block maxima, the max-lift from two rounds ago dropped). `probe.py` is the previous round's file with only its
FRAME paragraph, `BARS`/`BAR_VALUES` changed: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/`
and no MEASURE moved, so this round's numbers and the last round's compare directly.

## The complaint

> At the game camera the gas is still a sheet with dark horizontal bands. `row_dark_frac` reads 0.3822/0.3345/0.2204,
> worse than the previous round, and the probe dropped the bar instead of holding 0.08. Put `GasBankLoad` back to
> the plain mean, reshoot three captures, and bar `row_dark_frac` at or under 0.08. The bar has to sit outside the
> spread of three captures of the current build.

## The lever

One thing moves: `GasBankLoad` is the plain mean of the nine 4 m block maxima again, not `max(own, mean)`. The
max-lift (`d1c8e5f98c`) raised a fringe cell to its neighbourhood's load instead of trimming it, and that lifted
ring's own banding was the named suspect for `row_dark_frac` jumping from the mean-only build's 0.0194/0.0066/0.0461
to 0.1463-0.3077, and then to 0.2204-0.3822 once the depth-lattice pair went on top. The mean trims the ring under
the 0.8 stand floor instead of lifting it. Everything else is untouched: the pair (`GasBankPairZ`,
`GasBankPairAlphaK`), `GasBankBandM`, `GasBankWidth`, `GasBankZOffset`, and the smoke bank (unchanged, `smooth:
false`). `TW.Tests.BurstRecipeTests.TheGasBankEdgeDrawsFromItsNeighbourhood` (restored verbatim from `cfcbde147e`)
guards the core invariant this lever depends on.

## It did not close the key either

`row_dark_frac` reads **0.3802 / 0.3345 / 0.0000** this round. That is not the goal of 0.08, and it is not even an
improvement in the direction the owner asked for: the spread across the three runs is large (0.3802), so a bar
that clears this build's own spread would have to sit at 0.3802 + 0.3802 = **0.7604 or above** -- far looser than
the 0.08 the owner wants, and looser than the previous round's dropped 0.47 too. The rule in this folder forbids
loosening a bar to fit a round's own captures, so **this round again drops the bar rather than fake one**;
`row_dark_frac` is reported (printed per run) but not in `BARS`.

`row_gap_deepest` reads **0.5885 / 0.6142 / 0.2134**, about the same range as the last four rounds. It stays
unbarred for a fifth round running: a bar that clears this build's own spread (0.4008) would have to sit at
0.6142 + 0.4008 = **1.0150 or above**, while the previous round's captures (max 0.6330) sit under that value and
would PASS it rather than fail. Neither half of the bar rule overlaps the other for this key, so it stays reported,
not faked.

**Why the mean did not close the rows.** The mean trims the fringe ring's thickness back under the stand floor
instead of lifting it to its neighbourhood's level, so the outermost ring of a cloud is thinner and drops out of
the mask sooner -- that is what the plan expected to cost `bank_patch_count`, not what was expected to fix the row
seams. The row seam itself is the gap between one 4 m card-row's painted band and the next, which `GasBankLoad`
does not touch: it changes how thick a card draws, not how far apart the rows sit. The depth-lattice pair (closing
half the row pitch) and the mean (trimming the fringe) are two different levers on two different parts of the
same picture, and neither one is the row-pitch lever the owner's key actually needs.

**The patch ceiling loosens, as predicted.** `bank_patch_count` reads **19 / 17 / 18** against the pair-only
round's 18 / 17 / 16 -- the worst count is 19 and the spread is 2, so this round's own rule gives a ceiling of
**21**, looser than the pair-only round's 20. The commit message (`e373c114df`) named this cost up front: the mean
trims the fringe ring under the stand floor instead of merging it into the body, so some near-threshold rings drop
out and fragment into their own small patches again. This is reported, not hidden, and is not traded against the
row keys.

## The bars

Every value from `probe.json`, t1 sim_tick 556 / 636 / 716. Rule unchanged: a bar must clear the nearest run by
at least the three runs' own spread (`le` bar = max + spread, `ge` bar = min − spread, rounded away from the runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_patch_count` | 19 / 17 / 18 | 2 | le 21 | 2 | 18 / 17 / 16 (bar was 20) |
| `bank_paint_share` | 0.9009 / 0.8893 / 0.7707 | 0.1302 | ge 0.64 | 0.1307 | 0.9094 / 0.8728 / 0.8325 |
| `gas_paint_frac` | 0.0229 / 0.0210 / 0.0205 | 0.0024 | ge 0.018 | 0.0025 | 0.0243 / 0.0201 / 0.0208 |
| `gas_green_share` | 0.5188 / 0.5125 / 0.6516 | 0.1391 | ge 0.373 | 0.1395 | 0.5227 / 0.5058 / 0.6647 |
| `bank_low_share` | 0.0678 / 0.0549 / 0.0732 | 0.0183 | ge 0.036 | 0.0189 | 0.0651 / 0.0731 / 0.0768 |
| `bank_height_frac` | 0.3524 / 0.3445 / 0.3039 | 0.0485 | le 0.401 | 0.0486 | 0.3540 / 0.3073 / 0.3203 |
| `o240_area_frac` | 0.0253 / 0.0281 / 0.0303 | 0.0050 | ge 0.020 | 0.0053 | 0.0253 / 0.0280 / 0.0304 |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |

`row_dark_frac` stays **unbarred** this round (it has been barred at 0.47, then dropped entirely, before this
lever): runs 0.0000/0.3345/0.3802, spread 0.3802, so a bar that clears them sits at 0.7604 or above -- far looser
than the owner's goal of 0.08, so the bar is dropped rather than faked. `row_gap_deepest` stays **unbarred**, a
fifth round running: runs 0.2134/0.5885/0.6142, spread 0.4008, so a bar that clears them sits at 1.0150 or above,
while the previous round's captures (max 0.6330) sit under that value and would PASS it. Neither half of the bar
rule overlaps the other for either key, so both are reported, not faked.

## What moved the wrong way

- **`row_dark_frac`** did not close and is not reliably better than the pair-only round: 0.0000-0.3802 this round
  against 0.2204-0.3822 last round. The low end dropped to 0.0000 on run3, but run1 and run2 both still read above
  0.33 -- the goal of 0.08 is not in reach of any run. The bar is dropped rather than loosened (see above).
- **`bank_patch_count`** loosened, as the lever's own risk predicted: 17-19 against 16-18 last round, ceiling 21
  against 20.

## What moved the right way

- **`row_gap_deepest`**'s run3 value dropped to 0.2134 (against 0.4660-0.6396 across the last four rounds), but
  run1 and run2 stay in the same 0.59-0.61 range as before, so this reads as this run's own variance, not a trend;
  the key stays unbarred.
- Every other bar (`bank_paint_share`, `gas_paint_frac`, `gas_green_share`, `bank_low_share`, `bank_height_frac`,
  `o240_area_frac`) sits close to the previous round's own captures, consistent with the mean changing only the
  fringe ring's thickness and not the bank's overall paint, height or area.

## The eye check

`run1/gas-t1.jpg` at the game camera: the gas still reads as a wide green sheet with a visible dark horizontal
seam across its own height, the same complaint the owner raised against the pair-only build. The mean trims the
fringe ring's own banding under the stand floor, which is why `bank_patch_count` loosens (a thinner ring drops out
of the mask and fragments), but the seam between card-rows inside the body of the cloud is unaffected by this
lever, consistent with `row_dark_frac` not closing.

## Checks

- `python probe.py` → exit 0, every bar OK, both isolation cards REJECTED.
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g2` → exit 1, as it must.
- `python probe.py --selftest` → exit 0 (the camera maths against a hand calculation).
- `TW.Tests.BurstRecipeTests.TheGasBankEdgeDrawsFromItsNeighbourhood` → passes (restored verbatim, `e373c114df`).

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing. The owner's key,
`row_dark_frac` at or under 0.08, is **still not closed** after four different levers across four rounds (the
plain neighbourhood mean, the max-lift, the depth-lattice pair, and now the mean again with the pair on top of
it). Two things each lever has changed -- how thick a card draws (`GasBankLoad`) and how deep two cards in one
cell sit (the pair) -- have both been tried; neither touches the actual gap between one 4 m card-ROW and the
next, which is what a row seam is. The next lever this evidence points toward, not attempted here, is closing
that row-pitch gap directly (more cards per row, or cards wide enough in Z to overlap their own row neighbours,
not just their own cell's pair) rather than a further depth or thickness trick layered on the same two cards.

Out of scope here: any code or test change beyond what already landed with `e373c114df`, the smoke bank,
`GasBankBandM`/`GasBankWidth`/`GasBankZOffset`/the pair, reshooting `cards/`, `tasks.md`'s probe citation, turning
the sheet into a puff, and landing the lane.
