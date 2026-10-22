# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1: thickness from the neighbourhood

Three fresh captures of the neighbourhood-thickness build (`CombatFx.Banks.cs` at `cfcbde147e`), with every bar
in `probe.py` re-set from these three runs alone. `probe.py` is the previous round's file with only its FRAME
paragraph and `BAR_VALUES` changed: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/` and
no MEASURE moved, so this round's numbers and the last round's compare directly.

## The complaint

> One gas bank is still 28, 24 and 18 patches. The ceiling of 38 is the worst count plus that spread of 10, so a
> shredded sheet passes. Close the edge islands until the bank is one patch, and put that ceiling outside the
> spread of three captures.

## The lever

A card's thickness no longer comes from its own 4 m cell. `GasBankLoad` reads the NEIGHBOURHOOD: the mean of the
nine 4 m block maxima around the card. A lone thin cell at the cloud's fringe is pulled down by its eight empty
neighbours and falls under the stand floor, so it never draws; a cell one ring in is held up by its neighbours
and draws well above the probe's visibility bar. The aim was to delete the ring of near-threshold blotches that
the probe was counting as separate patches. The smoke bank is untouched (`smooth: false`).

## It did not work

`bank_patch_count` is **29 / 18 / 21** against the previous round's 28 / 24 / 18. The worst count is **29** and
the spread is **11**, so this folder's own rule gives a ceiling of **29 + 11 = 40** — *looser* than last round's
38, not tighter. The bank is not one patch and the ceiling is not tight. The bar is set where the captures
support it and not a pixel lower; no bar was loosened or tightened to make the count look better.

**Why the trim did not close the islands.** Dropping the fringe moves the cloud's edge inward by one ring, but
the new edge is itself a ragged line of cards whose soft skirts still fall below the paint threshold between
them. The islands are not a property of *which* ring is outermost; they are a property of *any* outermost ring
drawn at low opacity. Trimming cannot fix that — it just makes a new one.

**The next lever: a max-biased blend.** Instead of `mean(nine block maxima)`, use `max(own, mean)`. That LIFTS a
thin fringe cell to at least its neighbourhood's thickness rather than trimming it, so the outermost ring draws
opaque enough to join its neighbours instead of breaking into blotches. Same function, same file, one line.

## The bars

Every value from `probe.json`, t1 sim_tick 557 / 637 / 717. Rule unchanged: a bar must clear the nearest run by
at least the three runs' own spread (`le` bar = max + spread, `ge` bar = min − spread, rounded away from the
runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_patch_count` | 29 / 18 / 21 | 11 | le 40 | 11 | 28 / 24 / 18 (bar was 38) |
| `bank_paint_share` | 0.7625 / 0.8295 / 0.7168 | 0.1127 | ge 0.60 | 0.1168 | 0.7988 / 0.7711 / 0.8165 |
| `gas_paint_frac` | 0.0216 / 0.0200 / 0.0212 | 0.0016 | ge 0.018 | 0.0020 | 0.0213 / 0.0210 / 0.0198 |
| `gas_green_share` | 0.4726 / 0.4688 / 0.6139 | 0.1451 | ge 0.32 | 0.1488 | 0.4634 / 0.4543 / 0.4641 |
| `bank_low_share` | 0.0829 / 0.0723 / 0.0873 | 0.0150 | ge 0.05 | 0.0223 | 0.0746 / 0.0777 / 0.0785 |
| `bank_height_frac` | 0.2725 / 0.3055 / 0.2939 | 0.0330 | le 0.34 | 0.0345 | 0.3132 / 0.2814 / 0.2745 |
| `o240_area_frac` | 0.0209 / 0.0234 / 0.0254 | 0.0045 | ge 0.016 | 0.0049 | 0.0182 / 0.0204 / 0.0211 |
| `row_dark_frac` | 0.0194 / 0.0066 / 0.0461 | 0.0395 | le 0.09 | 0.0439 | 0.0325 / 0.0461 / 0.0200 |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |

`row_gap_deepest` stays **unbarred**. The runs are 0.2502 / 0.2761 / 0.2675, spread 0.0259, so a bar that clears
them must sit at 0.3020 or above, while leaving the previous round's captures (0.2542 / 0.2686 / 0.2582, min
0.2542) failing by more than that spread needs a bar under 0.2283. No value does both, so there is no honest
bar, and none is invented.

## What moved the wrong way

- **`bank_paint_share`** fell: 0.7168 at worst, against 0.7711 last round. The trimmed fringe takes paint out of
  the main patch without taking the islands with it.
- **`gas_green_share`'s spread blew out** from 0.0098 to 0.1451, because run3 reads 0.6139 against the other
  two's ~0.47. That forced the bar from 0.44 down to **0.32** — a real loosening, reported, not hidden. The
  isolation cards still fail it: `cards-smoke` 0.0908 and `cards-puff` 0.1076, under the bar by 1.58× and 1.46×
  the spread.
- **`bank_height_frac`** is 0.3055 at worst against 0.3132, so its bar loosened from 0.36 to 0.34 — that one is
  a tightening, in our favour.
- `gas_paint_frac` (0.0200-0.0216 against 0.0198-0.0213), `o240_area_frac` (0.0209-0.0254 against
  0.0182-0.0211) and `bank_low_share` (0.0723-0.0873 against 0.0746-0.0785) did **not** fall with the trimmed
  fringe, which the plan expected them to. The cloud did not shrink, so the stand-floor recovery (dropping
  `DrawGasBank`'s 0.8) was not needed and was not used.

**`bank_width_m`**: 37.59 / 33.78 / 31.40 m, against last round's 31.6-36.0 m. Wider at the top of the range,
roughly unchanged at the bottom. Unbarred, as before.

## The eye check

`run1/gas-t1.jpg` at the game camera: the gas reads as **one wide yellow-green sheet** lying over the mud, running
unbroken from the left shell holes across to the sandbag line on the right. The horizontal row banding the owner
complained about earlier is gone — there is one dark notch across the lower middle where the ground itself dips,
not a lattice of rows. What is still wrong is the **bottom edge**: it is a torn, feathered fringe rather than a
soft falloff, and those torn tips are the islands the probe counts.

## Checks

- `python probe.py` → exit 0, every bar OK, both isolation cards REJECTED.
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2` → exit 1, as it must.

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing.

Out of scope here: reshooting `cards/`, the smoke bank, the gas hue and its constants, `GasBankThin`/`GasBankFull`,
turning the sheet into a puff, and landing the lane.
