# ability-fx-captures-r2-2-g2-g2-g2: bar the rise, count O240's clouds, 2026-10-21

**`python probe.py` clears, exit 0.** No C# changed in this unit, so `-g2-g2`'s tip (74a1786411) is the current
build; its three runs ARE three captures of the current build, and this leg copied them unchanged.

## The owner's complaint

> Bar `green_rise_m`. It reads 0.359, 0.359 and 0.357 and is not in the bars, so a flat sheet passes.
> `green_area_frac` is 0.0055, 0.0056 and 0.0055 against a 0.005 bar, spread 0.0001. The o240 still is a green
> wash of about 10000 px, not one cloud. Start from `lane/show/ability-fx-captures-r2-2-g2-g2`'s tip. Bar outside
> the spread of three captures of the current build.

Both faults were real. `-g2-g2` computed `green_rise_m` in `measure_t1` and printed it, but never put it in
`BARS`, so a sheet with zero rise still passed. And O240 had no `largest_blob` (five calls 40 m apart are five
patches, one blob would keep only one), so `o240_green_area_frac` counted every green pixel, wash included, as if
it were gas.

## What is here

- `probe.py` — carries `-g2-g2`'s camera maths, measure (`greenness`, `greener`, `green_mask`) and T1's
  `largest_blob` unchanged. Adds `green_rise_m` to `BARS`. Pulls `largest_blob`'s component search out into
  `components(mask, w, h, dilate)`, returning every connected component biggest-first (`largest_blob` is now
  `components(...)[0]`, kept by python's stable sort, so T1's numbers do not move). `measure_o240` keeps the
  components with at least `CLOUD_MIN_PX` (50) member pixels as `clouds`, and reports `o240_cloud_count`,
  `o240_cloud_share` (clouded green pixels / all green pixels) and a redefined `o240_green_area_frac` (clouded
  pixels' share of the frame, not every green pixel's). `o240_blob_top` (the ten biggest component sizes) is an
  unbarred diagnostic.
- `run1/`, `run2/`, `run3/` — copied unchanged from `-g2-g2`: the four `GasBankStills` captures (`gas-t1`,
  `gas-t1-plate`, `gas-o240`, `gas-o240-plate`) plus their sidecars and manifests, at three different `wait_frames`
  (200/215/230), so three different frames. No Unity run was needed for this unit: nothing in the project changed
  since `-g2-g2`'s captures, so they already are three captures of the current build.
- `cards/` — the four `GasBankCards` isolation stills, carried from `-g2-g2`, for the unbarred look report only.
- `probe.json` — all three runs' numbers, frames, the look report and `BAR_VALUES`.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.30 | 1.0000, 1.0000, 1.0000 | 0.0000 | 0.7000 | owner-decided, carried from `-g2-g2` |
| `green_height_frac` | `le` | 0.15 | 0.0116, 0.0116, 0.0115 | 0.0001 | 0.1384 | owner-decided, carried from `-g2-g2` |
| `green_rise_m` | `ge` | 0.30 | 0.359, 0.359, 0.357 | 0.002 | 0.057 | the bank sits on the ground: a round number well under the floor 0.355; a flat sheet (rise 0) fails it |
| `green_area_frac` | `ge` | 0.005 | 0.0055, 0.0056, 0.0055 | 0.0001 | 0.0005 | unchanged; margin 0.0005 is five times the spread 0.0001, already outside it |
| `o240_green_area_frac` | `ge` | 0.008 | 0.0094, 0.0093, 0.0098 | 0.0005 | 0.0013 | clouded-pixel share, measured fresh with `CLOUD_MIN_PX`; the round number under the floor 0.0088 |
| `o240_cloud_count` | `ge` | 10 | 20, 15, 20 | 5 | 5 | the round number under the floor 10 |
| `o240_cloud_share` | `ge` | 0.85 | 0.8720, 0.8696, 0.8847 | 0.0151 | 0.0196 | the round number under the floor 0.8545 |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |

## What the O240 clouds look like

`--calibrate` on `CLOUD_MIN_PX = 50`: each run's green mask breaks into ~31-33 connected components, of which
~15-20 clear 50 px. The ten biggest per run (`o240_blob_top`), e.g. run1 `[4275, 2310, 620, 145, 121, 120, 119,
116, 89, 82]`, show two large clouds (the two closest of the five calls, bigger in frame because nearer the
camera) plus a tail of medium ones — not the "about 10000 px" wash read as a single blob, but not five crisp
clouds either: `o240_cloud_share` 0.87-0.88 means most (not all) of the green sits in components of 50 px or
more. The bars above are set from this measurement, as regression bars; they do not say the O240 look is final.

## The look report (unbarred)

From `cards/`: gas relief 0.0868 against the smoke card's 0.1933 (ratio 0.4490); both cards sit at ground level in
this isolation pose, so the rise ratio is 0/0 and says nothing. This is a look reading, not a bar — the gas card
is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, and changing how the bank LOOKS is out of this unit's
scope.

## Status: done

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (the camera hand calculation, 0.34311 m).
A flat sheet (rise 0) fails `green_rise_m`'s bar, as required.
