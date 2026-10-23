# ability-fx-captures-...-g1-g1-g1-g1: two cards per cell close the islands, not the bands

Three fresh captures of the two-sub-row build (`CombatFx.Banks.cs`, commit `164871e959`), with every bar in
`probe.py` re-set from these three runs alone. `probe.py` is the previous round's file with only its FRAME
paragraph, the `row_gap_deepest` note and `BAR_VALUES` changed: `CARDS_DIR` still points at
`../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so this round's numbers and the last round's
compare directly.

**Both of the owner's caps are MISSED. This round is BLOCKED, not done.** Neither cap was loosened to fit the
build; see "The two caps" below and the next lever.

## The complaint

> At the game camera the gas is still a striped sheet. The three close stills (sim ticks 557, 597, 637) have dark
> horizontal bands, and `bank_patch_count` is 19/13/6. The ceiling of 32 is 19 plus a spread of 13, so a 19-island
> sheet passes. Close the bands so it is one bank, reshoot three captures, and bar `row_gap_deepest` at or under
> 0.15 and `bank_patch_count` at or under 8. The bar has to sit outside the spread of three captures of the
> current build.

## The lever

The bands were measurable, not taste. One card stood anywhere inside its 4 m cell, so consecutive card centres
down a screen column were `4 + off(k+1) - off(k)` apart - between **2 and 6 m**. At the T1 pose (pitch 25 deg) a
6 m step rises

    6 * sin25 = 2.535 m

while a card's painted band only reaches

    1.78 * cos25 = 1.613 m,

so the widest pairs could not meet at any opacity. Every standing cell now stands `GasBankSubRows` = **2** cards,
one per half-cell stratum with `GasBankSubJitterM` = 1 m inside it, so the worst step is

    GasBankWorstDepthStep(1) = 3 m  ->  3 * sin25 = 1.268 m,

under the 1.613 m a band reaches (equivalently `1.613 / sin25 = 3.82 m` of depth step is the limit, and 3 < 3.82).
Coverage is now by construction, not by the hash's luck. The pair's hashes differ (`h ^ sub * 0x9E3779B9`) so the
two cards differ in width, roll, mirror and frame boil and do not read as twins. `GasBankLoad`,
`GasBankStandFloor` 2.4, `GasBankThin`/`GasBankFull`, `GasBankBandM`, `GasBankWidth` and the smoke bank (one card
a cell) are untouched: one lever this round.

## The two caps

| Owner's key | Cap | Runs | Spread | Rule's bar | Verdict |
|---|---|---|---|---|---|
| `bank_patch_count` | le 8 | 7 / 8 / 5 | 3 | 8 + 3 = **11** | **MISSED by 3** |
| `row_gap_deepest` | le 0.15 | 0.5930 / 0.5417 / 0.4656 | 0.1274 | 0.5930 + 0.1274 = **0.7204** | **MISSED by 0.57** |

`bank_patch_count` nearly closed: 7/8/5 against the raised-floor round's 19/13/6, a ceiling of 11 against 32 - and
against 38 / 40 / 46 in the three rounds before that. It misses only because the runs' own spread of 3 will not
fit under a cap of 8; the worst single run (8) is already at the cap. The bar in `BAR_VALUES` is the rule's **11**,
not a faked 8: the cap was not loosened and the bar was not tightened past what the runs support.

`row_gap_deepest` moved the **wrong way**, from 0.2807-0.2911 to 0.4656-0.5930. It stays out of `BARS` for a fifth
round. The two sub-rows join the sheet laterally (`bank_paint_share` 0.9353-0.9567, the highest ever recorded here)
but make each row's deepest dip worse, because twice the cards in depth means twice the alpha in the thick rows
and the same ground showing through where the field is thin.

## The bars

Every value from `probe.json`, t1 sim_tick 557 / 597 / 637. Rule unchanged: a bar must clear the nearest run by at
least the three runs' own spread (`le` bar = max + spread, `ge` bar = min - spread, rounded away from the runs).

| key | dir | runs | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|---|
| `bank_low_share` | ge | 0.0750 / 0.0830 / 0.0811 | 0.0080 | 0.066 | 0.0090 | 0.0738-0.0818 (bar 0.065) |
| `bank_height_frac` | le | 0.3290 / 0.3057 / 0.2946 | 0.0344 | 0.37 | 0.0410 | 0.2716-0.3007 (bar 0.33) |
| `o240_area_frac` | ge | 0.0252 / 0.0277 / 0.0283 | 0.0031 | 0.022 | 0.0032 | 0.0194-0.0220 (bar 0.016) |
| `pose_error_max` | lt | 0 / 0 / 0 | 0 | 0.5 | 0.5 | 0 (bar 0.5) |
| `gas_paint_frac` | ge | 0.0215 / 0.0197 / 0.0163 | 0.0052 | 0.011 | 0.0053 | 0.0161-0.0195 (bar 0.012) |
| `gas_green_share` | ge | 0.5376 / 0.4875 / 0.4786 | 0.0590 | 0.41 | 0.0686 | 0.4102-0.4357 (bar 0.38) |
| `bank_paint_share` | ge | 0.9567 / 0.9353 / 0.9524 | 0.0214 | 0.91 | 0.0253 | 0.8431-0.9188 (bar 0.76) |
| `bank_patch_count` | le | 7 / 8 / 5 | 3 | 11 | 3 | 19 / 13 / 6 (bar 32) |
| `row_dark_frac` | le | 0.3077 / 0.2510 / 0.2018 | 0.1059 | 0.42 | 0.1123 | 0.0331-0.0733 (bar 0.12) |
| `row_gap_deepest` | - | 0.5930 / 0.5417 / 0.4656 | 0.1274 | unbarred | - | 0.2807-0.2911 (unbarred) |

Five bars got tighter (`bank_low_share`, `o240_area_frac`, `gas_green_share`, `bank_paint_share`,
`bank_patch_count`). Two got looser. `gas_paint_frac`'s floor dropped only because run3's own value did.

## What moved the wrong way

- **`row_dark_frac` 0.2018-0.3077** against 0.0331-0.0733, so its bar loosened from 0.12 to **0.42**. This is the
  same fact as `row_gap_deepest`: the rows are more contrasty, not more even.
- **`bank_height_frac` 0.2946-0.3290** against 0.2716-0.3007, so its `le` bar loosened from 0.33 to **0.37**. The
  denser stack is taller on screen. It does **not** yet read as a solid poster (see the eye check), so the plan's
  named follow-up, lowering `GasBankFull` on height grounds, is not called for - though it is the next lever for
  a different reason, below.
- Card count rose as predicted: t1 `gas_cards` **494** against 247, o240 **192** against 96 - exactly 2x, well
  under `FlipbookFx.MaxCards` 1536. `draw_calls` is **133** at t1 (118-119 at o240), not above the old 133: one
  `DrawPacked` per book, as intended. Runs 2 and 3 shoot later ticks where the field has thinned, so their t1
  counts are 210 and 180.

## The eye check (`run1/gas-t1.jpg`)

The horizontal stripes are **gone**. The bank reads as one unbroken green sheet from the tank hull to the bottom
of the frame, with no dark mud lines crossing it - which is what `bank_patch_count` 7 and `bank_paint_share` 0.957
say. What is wrong now is a different thing: the sheet lies **flat on the ground like spilled paint**. It follows
every shell hole and duckboard instead of standing in front of them, so the depth the measure reads as a "row gap"
is really ground texture showing through a thin, low sheet. That is why `row_gap_deepest` got worse while the
picture got better.

## The next lever

Not another depth-step fix: the depth step is solved by construction. The sheet needs **body above the ground**.
In order of preference:

1. **Lower `GasBankFull`** (today 8 m) so an average cell reaches full opacity and the sheet stops being a thin
   wash the ground shows through. Cheap, one constant, and aimed straight at `row_gap_deepest`.
2. **Raise `GasBankBandM`** (the painted band height, today 1.78 m) so a card covers more screen vertical and the
   gaps between rows cannot read at all.
3. A `Kind.Flat` ground card per cell was considered and **rejected**: it spans 16-24 m of depth and cannot raise
   the gas over a man's head, which is the opposite of what the picture needs.

## The checks

- `python probe.py` -> **exit 0**, every bar OK, both isolation cards REJECTED (`cards-smoke` 0.0908,
  `cards-puff` 0.1076, both under the 0.41 bar by over 5x the runs' spread).
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1` -> **exit 1**: the
  old build's 19 patches fail the new bar of 11, as they must.
- Each run's `manifest.json`: `gas_cards` 2x, `draw_calls` not above 133, `pose_error_m` 0.

## This lane does not land

The puff brief `4101179359` is still unanswered, and both of the owner's caps for this round are missed. The lane
stays unlanded.
