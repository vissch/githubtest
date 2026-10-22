# ability-fx-captures-r2-2-g2-g2-g2-g2-g2: a gas puff at the close camera, not a sheet of stripes, 2026-10-22

**`python probe.py` clears, exit 0.** The drawing changed in this unit (`CombatFx.Banks.cs`), so run1/run2/run3
and cards/ are three fresh captures of THIS tree, not of any predecessor's.

## The owner's words

> Draw a gas puff at the close camera, not a sheet of pale green stripes. green_height_frac is 0.0116 and
> green_low_share is 1.0, and cards-gas.jpg is a flat puddle. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g2`'s tip. Bar outside the spread of three captures of the current
> build.

The owner's own sentence named the look — a puff, not stripes — so no concepts brief was filed for it.

## What changed in the drawing

`CombatFx.PackGasCell` (`Presentation/Camera/CombatFx.Banks.cs`), close up (step 1) only; the overview (step 2)
card is byte-identical to before, so every O240 bar measures the same drawing it always did.

- **the card's height is jittered by the cell's own hash:** `width * GasBankSquash * (1.15 + 0.4 * h4)`, clamped
  by `(GasHeadRoom + GasBankAnchorDrop) / GasInkTop` — the widest cells meet that clamp exactly, so no cell's ink
  tops `GasHeadRoom` 1.7 m. The flat build drew every cell at one height, which is why every column's p45 stood
  at the same 0.359 m.
- **three cells in eight carry a crown puff:** a second, near-square card at its own phase, lifted
  `0.15 + 0.45 * h3` off the ground and clamped so its ink too stops at 1.7 m. The T1 capture draws 1051-1066
  cards of `FlipbookFx.MaxCards` 1536, and `t1_gas_cards` is barred at 1100 so the crown cannot push it there.
- `draw_calls` at T1 is 133, the same as the flat build's.

`BurstRecipeTests.TheGasBankReadsAsAPuff_NotAFlatSheet` is the guard (hand-worked numbers, not read back out of
`PackGasCell`): the tallest cell over 1.20 m, no cell over `GasHeadRoom`, the cells' tops spread by at least
0.25 m, and about three cells in eight crowned. All 20 `BurstRecipeTests` are green.

## What is here

- `probe.py` — carries `-g2-g2-g2-g2`'s camera maths (`ray`, `ground_distance`, `height_at`, `selftest`), its
  measure (`greenness`, `greener`, `green_mask`), `largest_blob`, the frame guard, `pack`, `--calibrate` and
  every O240 key unchanged. New: `KNEE_M` 0.5 m and `green_knee_share` (the puddle bar), `green_top_rough` (an
  unbarred diagnostic, see below) and `t1_gas_cards` (from `manifest.json`'s `gas-t1.png` row).
  `green_height_frac` flips from `le` to `ge` — it is a floor saying the sheet must stand, and `green_low_share`
  carries the head rule it used to carry.
- `run1/`, `run2/`, `run3/` — three `GasBankStills` captures of this tree, `TW_GAS_WAIT_FRAMES` 200/215/230,
  each a gas pass and a `TW_GAS_PLATE=1` plate pass, four stills with sidecars and both manifests.
- `cards/` — a fresh `GasBankCards` isolation pass (gas/smoke/puff/plate), the unbarred look report only.
- `probe.json` — all three runs' numbers, frames, the look report and `BAR_VALUES`.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | the flat build read | why |
|---|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.88 | 0.9190, 0.9044, 0.9189 | 0.0146 | 0.0244 | 1.0000 | the head rule, now that the bank stands. The plan guessed 0.90 from the flat build's 1.0000; the puff reads 0.904-0.919, so 0.90 sits inside the noise and the floor under it is 0.88 |
| `green_height_frac` | `ge` | 0.013 | 0.0148, 0.0161, 0.0160 | 0.0013 | 0.0018 | 0.0115-0.0116 (fails) | a FLOOR now, not a ceiling: the sheet must stand |
| `green_rise_m` | `ge` | 0.44 | 0.493, 0.536, 0.532 | 0.0430 | 0.0530 | 0.357-0.359 (fails) | the median column's p45 height |
| `green_area_frac` | `ge` | 0.012 | 0.0127, 0.0126, 0.0130 | 0.0004 | 0.0006 | 0.0055 (fails) | the bank covers more of the frame |
| `green_knee_share` | `le` | 0.43 | 0.4063, 0.3949, 0.4071 | 0.0122 | 0.0229 | 0.4888-0.4946 (fails) | THE PUDDLE BAR: gas under `KNEE_M` 0.5 m over all gas |
| `t1_gas_cards` | `le` | 1100 | 1066, 1060, 1051 | 15 | 34 | 967 | the crown's cards against `MaxCards` 1536 |
| `o240_green_area_frac` | `ge` | 0.0075 | 0.0078, 0.0079, 0.0081 | 0.0003 | 0.0003 | the same | O240 is unchanged |
| `o240_large_cloud_count` | `ge` | 1 | 3, 3, 3 | 0 | 2 | 5, 3, 3 | carried from `-g2-g2-g2-g2`: three captures of a build do not always agree on the count, so `o240_large_share` is the real guard |
| `o240_large_share` | `ge` | 0.71 | 0.7264, 0.7373, 0.7298 | 0.0109 | 0.0164 | the same | O240 is unchanged |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | the same | `GasBankStills.PoseBar` |

## `green_top_rough` is a diagnostic, not a bar

The plan asked for it as a `ge` bar. It is printed and never barred, as `-g2-g2-g2-g2` left
`o240_large_cloud_count`'s thin margin: this build reads 0.4613, 0.4618 and 0.5204, a spread of 0.0591 that all
but swallows the margin to any bar, and the flat build read 0.404-0.437 — so no bar on it both clears the noise
and fails the sheet. `green_knee_share` and `green_rise_m` carry that job instead, and both fail the flat build.

## The open question for the owner

The plan's step 9 said to keep tuning until `green_rise_m` reaches 0.6 m. It does not: two rounds of tuning
(jitter 0.85+0.5 -> 1.0+0.4 -> 1.15+0.4, crown share 1/4 -> 3/8, crown lift 0.10+0.35 -> 0.15+0.45) moved the
median from 0.359 m to 0.53 m and stopped there, because `GasHeadRoom` 1.7 m caps the ink and raising the bank
further only pushes gas pixels over a man's head: `green_low_share` already fell from 1.0000 to 0.904-0.919 on
the way. 0.6 m and the head rule cannot both hold at `GasHeadRoom` 1.7. The row is in
`docs/reference/decisions.md` under the open questions; the default taken here is to keep the head rule and bar
the rise where this build reads it.

## Status

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (the camera hand calculation, 0.34311 m).
These are regression bars set from this build's own spread: they say the close bank is no longer the flat sheet,
not that the look is final.
