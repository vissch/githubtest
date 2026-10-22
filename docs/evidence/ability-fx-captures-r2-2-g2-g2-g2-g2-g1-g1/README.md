# ability-fx-captures-r2-2-g2-g2-g2-g2-g1-g1: one standing cloud at the far camera, 2026-10-22

**`python probe.py` clears, exit 0.** The standing card lands and its bars are re-derived. The second blob is
measured and printed but **not barred** — the captures say no honest bar exists yet (see below); the merge
decision is with the owner.

## The owner's complaint

> At the far camera the gas is still a cluster of pale green stripes, not one cloud a player can pick out. The
> biggest blob is 591, 620 and 540 px and the next is 238, 154 and 136, with 7 to 9 specks at or above 50 px.
> `LARGE_MIN_PX` 300 counts only the first, so `o240_large_cloud_count` reads 1, 1, 1 and the `le 1` bar has
> margin 0. Draw one standing cloud and bar the second blob so a stripe cluster fails. Bar outside the spread of
> three captures of the current build.

## What changed

- **The drawing (part 1, commit `0aab9ea`):** `CombatFx.FindGasClouds` flood-fills the gas field, and at
  `step > 1` (the overview) `DrawGasBank` draws **one card per connected cloud** — padded by `GasCloudPad` 4 m
  and squashed 0.8 (`GasCloudSquash`), not 0.55 — instead of a squashed pancake per 2x2 cell block. Guarded by
  `BurstRecipeTests.GasClouds_OneCardACloudFromTheOverview`.
- **`probe.py`:** `measure_o240` adds `o240_second_blob_px` (the second biggest component), printed on the
  per-run `o240_...` line. `o240_top_blob_px`, `o240_large_share` and `o240_green_area_frac` are re-derived from
  this tree's three fresh captures. `LARGE_MIN_PX` stays 300 (the owner refused lowering it in `-g2-g2-g2-g2`).
- **`cards/`** carried unchanged from `-g1` (no card drawing changed; the unbarred look report only).
- **`run1/`, `run2/`, `run3/`** — three fresh `GasBankStills` captures of this tree, `TW_GAS_WAIT_FRAMES`
  200/215/230, each the four stills plus sidecars and manifests, at six different frames.

## It worked: the far camera sees one body

| | `-g1` (a card per cell block) | here (one card a cloud) |
|---|---|---|
| biggest blob px | 591, 620, 540 | **1910, 1897, 2166** |
| `o240_large_share` | 0.3117, 0.3849, 0.2993 | **0.6925, 0.7974, 0.8025** |
| `o240_green_area_frac` | 0.0006, 0.0007, 0.0006 | **0.0021, 0.0021, 0.0024** |
| `o240_speck_count` | 7, 9, 8 | **3, 2, 3** |

Three times the pixels in the main blob, and two thirds to four fifths of all the green now sits inside a cloud a
player can pick out instead of a third.

## The second blob: measured, not barred

The three captures read `o240_second_blob_px` 238, 82 and 136 px — spread 156. The chain's rule (all three on the
pass side AND the nearest clears the bar by at least the spread) allows no `le` bar under **394 px**, which is
above the 238 px seen here and so would pass a stripe cluster too. A bar "well under 136 px", as the goal asks,
would fail this build's own run1. So the key is printed as a diagnostic and left out of `BARS` rather than set to
a number that prints a false green.

What the runner-up is **not**: the single card splitting on the sheet's own alpha holes. Raising the far band's
alpha floor to 0.9 (`GasCloudThin`, tried and reverted in this leg) moved the second blob by **0 px**
(238 -> 238) and `o240_large_share` by 0.004. What it **is**: in run1 a 29x29 px patch at x 320-348, y 399-427,
nowhere near the main cloud at x 591-685, y 332-360 — a second, genuinely separate sim cloud, drawn correctly as
its own card by `FindGasClouds`.

Barring it therefore needs a decision: merge neighbouring clouds into one card at the overview, or accept two
cards and bar only the main cloud. That is a look decision, so it is the owner's, and it is open in
`docs/reference/decisions.md` ("the second far gas blob"). Until then this folder's bars guard the win above.

## The bars

Checked twice, as always: all three runs on the pass side, AND the nearest value clears the bar by at least the
three runs' own spread.

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.30 | 0.9138, 0.9109, 0.9122 | 0.0029 | 0.6109 | carried, re-checked — still clears |
| `green_height_frac` | `le` | 0.15 | 0.0209, 0.0209, 0.0192 | 0.0017 | 0.1291 | carried, re-checked — still clears |
| `green_rise_m` | `ge` | 0.30 | 0.587, 0.587, 0.539 | 0.0480 | 0.2390 | carried, re-checked — still clears |
| `green_area_frac` | `ge` | 0.0030 | 0.0034, 0.0034, 0.0034 | 0.0000 | 0.0004 | carried from `-g1`, re-checked — T1 is drawn by the untouched `step == 1` path and did not move |
| `o240_green_area_frac` | `ge` | 0.0015 | 0.0021, 0.0021, 0.0024 | 0.0003 | 0.0006 | re-derived: the standing card covers 3x the frame share, so `-g1`'s 0.0004 is far too slack |
| `o240_large_cloud_count` | `le` | 1 | 1, 1, 1 | 0 | 0 | one cloud on every capture; paired with `o240_top_blob_px` so a reading of zero clouds still fails |
| `o240_top_blob_px` | `ge` | 1500 | 1910, 1897, 2166 | 269 | 397 | re-derived: the standing card against `-g1`'s 450 bar; a pancake cluster's 591 px biggest blob now fails |
| `o240_large_share` | `ge` | 0.55 | 0.6925, 0.7974, 0.8025 | 0.1100 | 0.1425 | re-derived: most of the green must sit in the one cloud, not in the wash |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |

`o240_second_blob_px` (238, 82, 136), `o240_speck_count`, `o240_blob_top`, `green_px`, `plate_diff_px`,
`blob_px`, `o240_green_px` and `o240_plate_diff_px` are printed per run and barred on nothing.

## The look report (unbarred)

From `cards/` (carried, unchanged): gas relief 0.0849 against the smoke card's 0.2178 (ratio 0.3898); both sit at
ground level in this isolation pose, so the rise ratio is 0/0 and says nothing. Out of this unit's scope
(`CombatFx.GasBankSquash`, card look).

## Status: the drawing done, the second-blob bar open

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (camera hand calculation, 0.34311 m).
`BurstRecipeTests` 20/20 green. The one thing the goal asked for and this folder does not deliver is the
second-blob bar; the paragraph above says why, with numbers, and the decision is with the owner.

Predecessor: `docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g2-g1/` (one O240 call, not a wash).
