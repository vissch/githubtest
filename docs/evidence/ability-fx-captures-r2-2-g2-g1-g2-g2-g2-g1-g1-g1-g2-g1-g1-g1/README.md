# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1: the raised stand floor closes the islands

Three fresh captures of the raised-stand-floor build (`CombatFx.Banks.cs`, commit `d36c420919`), with every bar in
`probe.py` re-set from these three runs alone. `probe.py` is the previous round's file with only its FRAME
paragraph, the `row_gap_deepest` note and `BAR_VALUES` changed: `CARDS_DIR` still points at
`../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so this round's numbers and the last round's
compare directly.

## The complaint

> The max blend did not join the sheet. At the game camera the three close stills (sim_tick 752, 866, 947) are
> still a striped green sheet with a ragged near edge and dark gaps, not one patch. `bank_patch_count` is 35/30/24
> and the ceiling moved from 40 to 46, so the probe exits 0 on a worse sheet. Raise `DrawGasBank`'s Refresh cell
> floor (the 0.8 stand floor) so a near-threshold outer ring does not draw as its own islands. Hold
> `bank_patch_count` at or under 38. Do not loosen the bar to this build's spread.

## The lever

`DrawGasBank`'s Refresh cell floor is `GasBankStandFloor` = **2.4 m**, up from 0.8 m: a cell under that load draws
no card at all. The thinnest card that still stands now draws

    0.19 + 0.43 * (2.4 / 8) = 0.19 + 0.43 * 0.3 = 0.319

against **0.233** before (`0.19 + 0.43 * 0.1`). 0.319 is past **0.31**, half the bank's core opacity — the same
haze/paint line `TheGasBankEdgeIsLiftedIntoTheMass` already uses. So no standing card sits at the probe's paint
threshold any more: the near-threshold outer ring that broke into blotches is simply not drawn. `GasBankLoad`'s
formula (max of own and neighbourhood mean) is unchanged, and the smoke bank's own 1.5 floor is untouched.

## It worked

`bank_patch_count` is **19 / 13 / 6** against the max-blend round's 35 / 30 / 24. The worst count is **19** and the
spread is **13**, so this folder's own rule gives a ceiling of **19 + 13 = 32** — inside the owner's cap of 38, and
tighter than every earlier round: 38 (plain cell), 40 (neighbourhood mean), 46 (max blend). No bar was loosened to
reach it.

## The bars

Every value from `probe.json`, t1 sim_tick 557 / 597 / 637. Rule unchanged: a bar must clear the nearest run by at
least the three runs' own spread (`le` bar = max + spread, `ge` bar = min - spread, rounded away from the runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_patch_count` | 19 / 13 / 6 | 13 | le 32 | 13 | 35 / 30 / 24 (bar was 46) |
| `bank_paint_share` | 0.8431 / 0.8497 / 0.9188 | 0.0757 | ge 0.76 | 0.0831 | 0.7621 / 0.8006 / 0.8458 |
| `gas_paint_frac` | 0.0195 / 0.0183 / 0.0161 | 0.0034 | ge 0.012 | 0.0041 | 0.0324 / 0.0424 / 0.0343 |
| `gas_green_share` | 0.4357 / 0.4102 / 0.4193 | 0.0255 | ge 0.38 | 0.0302 | 0.4616 / 0.3995 / 0.3725 |
| `bank_low_share` | 0.0738 / 0.0818 / 0.0804 | 0.0080 | ge 0.065 | 0.0088 | 0.0840 / 0.0791 / 0.0612 |
| `bank_height_frac` | 0.3007 / 0.2747 / 0.2716 | 0.0291 | le 0.33 | 0.0293 | 0.3433 / 0.2725 / 0.3112 |
| `o240_area_frac` | 0.0194 / 0.0215 / 0.0220 | 0.0026 | ge 0.016 | 0.0034 | 0.0213 / 0.0295 / 0.0347 |
| `row_dark_frac` | 0.0331 / 0.0733 / 0.0608 | 0.0402 | le 0.12 | 0.0467 | 0.3077 / 0.2677 / 0.1463 |
| `pose_error_max` | 0.0000 x3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |

`row_gap_deepest` stays **unbarred**, but for the first time it could be barred: the runs are 0.2807 / 0.2911 /
0.2830 (spread 0.0104), so an `le` bar at 0.31 would clear them and would also FAIL the previous round's captures
(0.4660-0.6396). The two halves of the rule finally overlap. Barring it is out of scope for this leg, so the key is
reported and the next leg can bar it at 0.31.

## What moved the wrong way

- **`gas_paint_frac` nearly halved**: 0.0161-0.0195 against 0.0324-0.0424, about 0.45x. A higher floor drops the
  whole outer skirt of cards, so the gas paints much less of the frame. Said plainly beside the patch count: part
  of the drop from 35/30/24 to 19/13/6 is the bank being **smaller**, not only better joined. The eye check below
  is what says it still reads as one sheet rather than a shrunken remnant, and `bank_paint_share` rising to
  0.84-0.92 says the paint that is left sits in one patch rather than in scattered pieces.
- **`bank_width_m`** (unbarred, in `probe.json`): 36.40 / 24.16 / 28.72 m against 36.37 / 33.36 / 31.14 m. The
  near-run width is unchanged but runs 2 and 3 are 5-9 m narrower, consistent with one or two outer rings falling
  under the floor.
- **`o240_area_frac`** fell: 0.0194-0.0220 against 0.0213-0.0347, bar 0.016 against 0.007. The overview shot's
  bank covers less of the frame for the same reason. (The bar itself tightened, because the three runs are much
  closer together than last round's.)
- **`bank_rise_m`** / **`bank_height_frac`** came down slightly (6.52-7.22 m against 6.44-7.77 m): the bank is a
  touch lower as well as narrower.
- Everything else moved the right way: `row_dark_frac` dropped roughly 4x (0.0331-0.0733 against 0.1463-0.3077),
  `row_gap_deepest` halved, `bank_paint_share` rose, and `gas_green_share` tightened from 0.28 back up to 0.38
  with both isolation cards still REJECTED by over 10x the runs' spread.

## The eye check

`run1/gas-t1.jpg` at the game camera: the gas reads as **one** continuous green sheet lying in the ground, with a
soft feathered near edge instead of the ragged line of lighter blotches the last three rounds showed. One darker
seam still crosses the lower half of the bank where two depth rows meet — that is the `row_dark_frac` 0.0331 and
the `row_gap_deepest` 0.28, much weaker than before but not gone. The sheet is visibly smaller than the max-blend
capture, matching `gas_paint_frac`.

## Checks

- `python probe.py` -> exit 0, every bar OK, both isolation cards REJECTED, `bank_patch_count` bar 32 (<= 38).
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1` -> exit 1, as it must
  (the old build's 35 patches fails the new bar of 32).

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing.

Out of scope here: the smoke bank's own floor, `GasBankThin`/`GasBankFull`, `GasBankLoad`'s formula, reshooting
`cards/`, `row_gap_deepest`'s bar, `tasks.md`'s probe citation, turning the sheet into a puff, and landing the lane.
