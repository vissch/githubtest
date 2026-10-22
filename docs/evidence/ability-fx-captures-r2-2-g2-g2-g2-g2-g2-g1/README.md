# ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1: the close gas is the smoke card's cloud, green, 2026-10-22

**`python probe.py` clears, exit 0.** The drawing changed in this unit, so `run1`/`run2`/`run3` — each a gas
pass, a no-gas plate pass and its own `GasBankCards` pass — are three fresh captures of THIS tree.

## The owner's words

> At zoom 30 the close gas is still a sheet of pale green stripes. cards-gas is a flat green puddle with two
> beams, cards-puff is a faint wisp, and cards-smoke is the only standing cloud. Draw the gas as that cloud, with
> a rim, and bar its rise and its relief against the smoke card. The height floor of 0.013 sits under a sheet of
> 0.0148 to 0.0161. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g2-g2`'s tip. Bar outside the spread
> of three captures of the current build.

The owner named the look himself — the smoke card's cloud, with a rim — so no concepts brief was filed.

## What changed in the drawing, and why the `Ink` row was the cause

The fault was in the sheet table, not in the card sizes. `FlipbookFx.Sheets` row `GasBank` has
`Ink = (0.00, 0.35)`: the drawing in `GasBank.png` only ever inks the bottom 35% of its cell, however tall the
card. `SmokeBank` has `Ink = (0.03, 0.87)` — it fills its cell, which is why `cards-smoke` was the only standing
cloud of the three. Jittering card heights (the last unit) could not beat that: a taller card drew a taller
stripe.

- a new `Book.GasCloud` row (`FlipbookFx.cs`) draws the SMOKE sheet with `GasBank`'s `Lit` and a green `Tint`,
  and `CombatFx.ApplyTints` tints it the same green as the bank (day and night);
- close up (step 1) `CombatFx.PackGasCell` packs into `Book.GasCloud` and boils in the smoke sheet's 22-frame
  range; from the overview (step 2) the card is the flat `GasBank` card it always was, byte-identical, so every
  O240 bar still measures the drawing it was set on;
- the card is squashed by `GasCloudSquash` **0.33** (times the cell hash's 0.8-1.2) and clamped so its ink stops
  at `GasHeadRoom` 1.7 m;
- three cells in eight carry a **rim** — the same cloud 1.12x wider, 1.5x brighter and nearly see-through — in
  place of the old crown card, one for one, so the card count did not move (1051-1066 of `MaxCards` 1536).

`BurstRecipeTests.TheGasBankReadsAsAStandingCloud_NotAFlatSheet` is the guard, hand-worked and not read back out
of `PackGasCell`. All 50 tests of `FlipbookOrdinalTests|BurstRecipeTests|KnobsTests|FarGrowTests` pass, including
`EveryRow_HasItsBook`.

**0.33 is the second round of tuning.** At `GasCloudSquash` 0.25 the three captures read `green_height_frac`
0.0151-0.0157 and at 0.28 they read 0.0158-0.0167 — neither clears the sheet build's top of 0.0161 with room to
spare. At 0.33 every capture reads 0.0183 or more. What that costs is in "the head rule got weaker" below.

## What is here

- `probe.py` — carries `-g2-g2-g2-g2-g2`'s camera maths (`ray`, `ground_distance`, `height_at`, `selftest`), its
  measure (`greenness`, `greener`, `green_mask`), `components`, `largest_blob`, `pack`, the frame guard,
  `--calibrate` and every O240 key unchanged. New: the cards are measured **per run** inside `measure_run`, and
  `gas_smoke_rise_ratio` and `gas_smoke_relief_ratio` are real bars (the predecessor printed them as an unbarred
  look report out of one top-level `cards/` folder, which is gone, with `look_report()`). `find_runs()` now also
  requires the four `cards-*` stills in each run.
- `run1/`, `run2/`, `run3/` — three captures of this tree, `TW_GAS_WAIT_FRAMES` 200/215/230: the four
  `GasBankStills` stills with sidecars and both manifests, plus that run's own four `GasBankCards` stills.
- `probe.json` — all three runs' numbers, their frames and `BAR_VALUES`.

## The card rise measure was broken, and is fixed here

The predecessor's `card_rise_m` returned **0.000 m for every card of every build**. It took each masked pixel's
own `ground_distance` and then asked `height_at` for the height at exactly that distance — the two are inverses,
so every pixel read 0 and so did the median. That is why `-g2-g2-g2-g2-g2`'s look report printed `cards-gas`
0.000 m, `cards-smoke` 0.000 m and a rise ratio of 0.0000: the numbers said nothing about either drawing.

The fix is the maths `measure_t1` already used: a column's distance comes from its FOOT (its lowest pixel on
screen) and every pixel in the column is measured in that column's vertical plane. Re-measured that way, the
sheet build's own cards read gas 0.601 m / smoke 0.226 m / relief 0.0811 against 0.2178 — see the bar table.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | what the sheet build read | why |
|---|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.59 | 0.8824, 0.8840, 0.7393 | 0.1447 | 0.1493 | 0.9044-0.9190 | the head rule, much weaker than the predecessor's 0.88: see below |
| `green_height_frac` | `ge` | 0.015 | 0.0183, 0.0184, 0.0214 | 0.0031 | 0.0033 | 0.0148-0.0161 | every run reads above the sheet's top, but the BAR cannot: see below |
| `green_rise_m` | `ge` | 0.52 | 0.620, 0.624, 0.714 | 0.0940 | 0.1000 | 0.493-0.536 | the median column's p45 height; the bar does not fail the sheet on its own |
| `green_area_frac` | `ge` | 0.017 | 0.0228, 0.0226, 0.0276 | 0.0050 | 0.0056 | 0.0126-0.0130 (**fails**) | the cloud covers nearly twice the frame the sheet did |
| `green_knee_share` | `le` | 0.39 | 0.3513, 0.3521, 0.3235 | 0.0286 | 0.0379 | 0.3949-0.4071 (**fails**) | THE PUDDLE BAR: gas under `KNEE_M` 0.5 m over all gas |
| `t1_gas_cards` | `le` | 1100 | 1066, 1060, 1051 | 15 | 34 | 1051-1066 | the rim replaces the crown one for one, against `MaxCards` 1536 |
| `gas_smoke_rise_ratio` | `ge` | 2.07 | 2.0752 x3 | 0.0000 | 0.0052 | 2.6593 | the gas card's rise over the smoke card's — and the sheet clears it too: see below |
| `gas_smoke_relief_ratio` | `ge` | 3.11 | 3.1125 x3 | 0.0000 | 0.0025 | 0.3724 (**fails by 8x**) | THE CLOUD BAR: the gas card's relief over the smoke card's |
| `o240_green_area_frac` | `ge` | 0.0075 | 0.0078, 0.0079, 0.0081 | 0.0003 | 0.0003 | the same | O240 is the unchanged drawing |
| `o240_large_cloud_count` | `ge` | 1 | 3, 3, 3 | 0 | 2 | 3, 3, 3 | carried: three captures of a build do not always agree on the count |
| `o240_large_share` | `ge` | 0.71 | 0.7273, 0.7377, 0.7314 | 0.0104 | 0.0173 | the same | O240 is the unchanged drawing |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | the same | `GasBankStills.PoseBar` |

### The two card bars have a spread of zero, and that is weaker than it looks

`GasBankCards` makes its own `FlipbookFx`, hand-packs one card per book from `now = 0` with a fixed hash and a
pinned clock, and photographs one pose. It is deterministic by construction, so all three runs read the same
2.0752 and 3.1125: that is **one measurement counted three times**, not three captures agreeing. The bars still
have to mean something on their own, and `gas_smoke_relief_ratio` does — 3.11 against the flat puddle's 0.37.

### `gas_smoke_rise_ratio` does not fail the sheet build

The sheet build's gas card reads **2.6593**, higher than this build's 2.0752. A wide card lying on the ground,
photographed at the capture's 25 deg pitch, covers vertical pixels that a one-camera measure reads as height —
exactly the "flat green puddle" the owner described. So the rise ratio is barred as the owner asked, and it does
guard the gas card against collapsing under the smoke card, but **the relief ratio is the bar that tells a
standing cloud from a puddle**. Making the rise ratio mean "stands up" needs a second, flat-on camera pose in
`GasBankCards`; that is logged in `docs/reference/decisions.md` and was not built here.

### `green_height_frac`: every run clears 0.0161, the bar cannot

The owner's point was that the floor of 0.013 sat under a sheet reading 0.0148-0.0161. This build reads
0.0183/0.0184/0.0214 — every capture is above the sheet's top. But the spread rule puts the bar at or under
min - spread = 0.0183 - 0.0031 = 0.0152, under 0.0161. Two rounds of tuning (0.25 -> 0.28 -> 0.33) did not fix
that: raising the squash raises the readings and the run-to-run spread together, because `TW_GAS_WAIT_FRAMES`
200/215/230 catch a taller cloud at more different stages of its life. Rather than set a bar inside this build's
own noise, the bar is 0.015 and this paragraph says plainly that it is `green_area_frac` (0.017 against the
sheet's 0.0130) and `green_knee_share` (0.39 against 0.40) that fail the sheet build on the T1 pixels.

### The head rule got weaker

`green_low_share` — the share of gas pixels at or under `HEAD_M` 1.8 m — was 0.904-0.919 on the sheet build and
is 0.739-0.884 here, so its bar fell from 0.88 to 0.59. `GasHeadRoom` is untouched at 1.7 m and the ink clamp
still holds every card's ink under it; what stands over a man's head in run3 is the measure's own reading of a
taller, more ragged cloud. The predecessor's open question ("how high may the close gas bank stand?") now has
numbers against it — the 0.6 m rise the plan wanted is reached, at this cost — and both it and the rise-measure
question are in `docs/reference/decisions.md` under the open questions, with the default taken.

`green_top_rough` stays a printed diagnostic, never a bar, as the predecessor left it: this build reads
0.4512-0.8370, a spread no bar clears.

## Status

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (the camera hand calculation, 0.34311 m).
These are regression bars set from this build's own spread: they say the close bank is drawn with the standing
cloud and is no longer the flat sheet, not that the look is final — the owner has not seen this build in Play.
