# ability-fx-captures-...-g1-g1-g1-g1: full load closes the islands; the row measure cannot reach its cap

Three fresh captures of the full-load build (`CombatFx.Banks.cs`, commit `f97e486eef`), with every bar in
`probe.py` re-set from these three runs alone. `probe.py` is the previous round's file with only its FRAME
paragraph, the `row_gap_deepest` note and `BAR_VALUES` changed: `CARDS_DIR` still points at
`../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so this round's numbers and the last round's
compare directly.

**`bank_patch_count` MEETS the owner's cap of 8. `row_gap_deepest` MISSES 0.15 and cannot reach it as the measure
stands - see "The cap no build can pass". This round is BLOCKED on that one key, and it needs an owner decision,
not a seventh lever.**

## The complaint

> At the game camera the gas is still a striped sheet. The three close stills (sim ticks 557, 597, 637) have dark
> horizontal bands, and `bank_patch_count` is 19/13/6. The ceiling of 32 is 19 plus a spread of 13, so a 19-island
> sheet passes. Close the bands so it is one bank, reshoot three captures, and bar `row_gap_deepest` at or under
> 0.15 and `bank_patch_count` at or under 8. The bar has to sit outside the spread of three captures of the
> current build.

## The lever: the full load IS the stand floor

The depth step was closed last round (`bank_patch_count` 7/8/5). What was left was a brightness RAMP across the
bank, and the previous round's own row profile said so. Over the paint's 5-95% column span, the mean luma lift of
the PAINTED pixels of that round's `run1/gas-t1.jpg`:

| image row | what it is | mean lift of painted px |
|---|---|---|
| 220 | the bank's far lip | 14.3 |
| 280-410 | the core | flat inside +-10% |
| 400 | the core's peak | 25.2 |
| 540 | the bank's near lip | 12.5 |

Nothing was MISSING between rows; the lip cards were half as bright as the core's. With `GasBankStandFloor` 2.4
and `GasBankFullM` 8, a standing card's `Thick` ran 0.3 to 1, alpha 0.319 to 0.62 and brightness
0.75 + 0.45 * `Thick` 0.885 to 1.2, so a lip card painted `0.319 * 0.885 / (0.62 * 1.2)` = **0.38** of a core card
(measured end/core 0.50).

`GasBankFullM` is now `GasBankStandFloor` itself, so `Thick` is exactly **1** for every cell that passes the
floor: one alpha 0.62, one brightness 1.2, and no near-threshold ring anywhere in the bank. The soft edge now
comes from how many cards OVERLAP at the fringe, not from each fringe card being faint. One lever this round:
`GasBankThin`/`GasBankFull`, `GasBankLoad`'s formula, `GasBankBandM`, `GasBankSubRows`, `GasBankWidth` and the
smoke bank are unchanged.

## The two caps

| Owner's key | Cap | Runs | Spread | Rule's bar | Verdict |
|---|---|---|---|---|---|
| `bank_patch_count` | le 8 | 2 / 5 / 3 | 3 | 5 + 3 = **8** | **MET** |
| `row_gap_deepest` | le 0.15 | 0.4473 / 0.4495 / 0.5207 | 0.0734 | 0.5207 + 0.0734 = **0.5941** | **MISSED by 0.44** |

`bank_patch_count` closed: 2/5/3 against 7/8/5 last round, and 19/13/6, 35/30/24, 38/40/46 before that. The bar in
`BAR_VALUES` is the rule's 8 - it lands on the cap by the rule, not by loosening the cap or tightening the bar
past what the runs support. `bank_paint_share` 0.9841-0.9899 is the highest recorded here.

## The cap no build can pass (new, and the thing the owner has to rule on)

`row_gap_deepest` improved (0.4473-0.5207 against 0.4656-0.5930) but is nowhere near 0.15, and the reason is now
measured, not guessed:

- **The plateau is already flat.** The middle 60% of the trimmed core dips only 0.1736 / 0.2142 / 0.2150 below its
  own median on the three runs.
- **The deepest row is always at an END of the core:** core row 0 of 320 on run2, 343 of 370 on run1, 297 of 299
  on run3. It is the cloud's own near/far silhouette, not a gap between cards.
- **The geometry, not the build, sets the floor.** `ROW_CORE_TRIM` is 0.15 of an extent that runs all the way down
  to `ROW_EXTENT_FLOOR` 0.25 of the peak, so the trim never removes the fall-off. A perfectly smooth dome scores
  **0.3033** (parabola) or **0.3360** (half-cosine) on this very measure - both twice the cap.

So no gas build can pass `row_gap_deepest` le 0.15 while the measure is defined this way. The honest options, for
the owner: (a) drop this cap and judge the bands by `bank_patch_count` / `row_dark_frac` / the eye, which all now
pass; (b) raise `ROW_EXTENT_FLOOR` or `ROW_CORE_TRIM` so the measure reads only the plateau, and set a cap against
the dome number; (c) keep the cap and accept that the lane stays blocked. A measure is never moved to fit a cap
inside a leg, so nothing here was touched.

## The bars

Every value from `probe.json`, t1 sim_tick 557 / 597 / 637. Rule unchanged: a bar must clear the nearest run by at
least the three runs' own spread (`le` bar = max + spread, `ge` bar = min - spread, rounded away from the runs).

| key | dir | runs | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|---|
| `bank_low_share` | ge | 0.0770 / 0.0813 / 0.0881 | 0.0111 | 0.065 | 0.0120 | 0.0750-0.0830 (bar 0.066) |
| `bank_height_frac` | le | 0.4061 / 0.3490 / 0.3183 | 0.0878 | 0.50 | 0.0939 | 0.2946-0.3290 (bar 0.37) |
| `o240_area_frac` | ge | 0.0284 / 0.0316 / 0.0322 | 0.0038 | 0.024 | 0.0044 | 0.0252-0.0283 (bar 0.022) |
| `pose_error_max` | lt | 0 / 0 / 0 | 0 | 0.5 | 0.5 | 0 (bar 0.5) |
| `gas_paint_frac` | ge | 0.0329 / 0.0270 / 0.0231 | 0.0098 | 0.013 | 0.0101 | 0.0163-0.0215 (bar 0.011) |
| `gas_green_share` | ge | 0.6687 / 0.6207 / 0.6017 | 0.0670 | 0.53 | 0.0717 | 0.4786-0.5376 (bar 0.41) |
| `bank_paint_share` | ge | 0.9899 / 0.9841 / 0.9861 | 0.0058 | 0.97 | 0.0141 | 0.9353-0.9567 (bar 0.91) |
| `bank_patch_count` | le | 2 / 5 / 3 | 3 | 8 | 3 | 7 / 8 / 5 (bar 11) |
| `row_dark_frac` | le | 0.0838 / 0.1781 / 0.1070 | 0.0943 | 0.28 | 0.1019 | 0.2018-0.3077 (bar 0.42) |
| `row_gap_deepest` | - | 0.4473 / 0.4495 / 0.5207 | 0.0734 | unbarred | - | 0.4656-0.5930 (unbarred) |

Seven bars got tighter. One got looser.

## What moved the wrong way

- **`bank_height_frac` 0.3183-0.4061** against 0.2946-0.3290, so its `le` bar loosened from 0.37 to **0.50**. Full
  alpha on every card lifts the bank's faint top edge above the mask's threshold, so the bank reads taller. It is
  not one of the owner's caps; it is re-set from the runs, not tuned. `bank_rise_m` is 6.8-8.2 m and
  `bank_low_share` 0.077-0.088 still passes its own bar.
- Nothing else moved the wrong way. `row_dark_frac` more than halved (bar 0.42 -> 0.28) and `gas_green_share` rose
  by a fifth: less pale paint reads as more green.
- Card counts are unchanged, as predicted - the same cells stand, they just draw at one opacity: t1 `gas_cards`
  **494** (runs 2 and 3 shoot later, thinner ticks: 210 and 180), o240 192/214/226, `draw_calls` **133** at t1 and
  118-119 at o240, `pose_error_m` 0 everywhere.

## The eye check (`run1/gas-t1.jpg`)

No horizontal stripes and no dark mud lines: one unbroken green sheet from the tank hull to the bottom of the
frame, with a soft, ragged edge where the cards thin out - which is what `bank_patch_count` 2 and
`bank_paint_share` 0.990 say. It does **not** read as a hard green poster, so the plan's `GasBankFullM = 3f`
fallback was not needed and was not shot.

What is still wrong is the thing last round named and this lever did not aim at: the sheet lies **flat, like
spilled paint on the ground**, following shell holes and duckboards instead of standing in front of them. That is
a body-above-the-ground problem (`GasBankFull`, `GasBankBandM`), out of this round's scope.

## The checks

- `python probe.py` -> **exit 0**, every bar OK, both isolation cards REJECTED (`cards-smoke` 0.0908 and
  `cards-puff` 0.1076, both under the 0.53 bar by over 6x the runs' spread).
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1` -> **exit 1**.
- Each run's `manifest.json`: `gas_cards` 494 at t1, `draw_calls` not above 133, `pose_error_m` 0, three different
  t1 sim ticks (557 / 597 / 637).
- The edit gate green on this exact tree.

## This lane does not land

The puff brief `4101179359` is still unanswered, and `row_gap_deepest` misses the owner's cap. The lane stays
unlanded.
