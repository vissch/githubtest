# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2: a cloud at the overview a player can see, 2026-10-22

**Part 1 of 3, done.** `python probe.py` exits 0 on `run1/`, `run2/`, `run3/` here - the parent's captures,
carried whole, so the folder stands on its own. Part 1 adds one measure and **no bar**:
`o240_visible_clouds` reads **0, 0 and 0** on the current build. That zero IS the owner's complaint, measured.
Parts 2 and 3 move the build and then bar the count.

## The owner's complaint

> At the overview the chlorine bank is invisible. gas-o240 run1, run2 and run3 (frames 341, 356 and 371) are the
> same night map, and `o240_green_px` is 1909, 1656 and 1616. The ceiling of 15 counts specks a player cannot
> see. Put a cloud on that camera that a player can see, and bar that visible count outside the spread of three
> captures of the current build. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1`'s tip.

## He is right, and the fix is card SIZE

The parent's `probe.json` reads `o240_blob_top` (the ten biggest components of the O240 green mask, run1):

| rank | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|
| run1, px | 119 | 94 | 91 | 91 | 86 | 81 | 76 | 75 | 68 | 67 |
| run2, px | 117 | 104 | 91 | 79 | 72 | 71 | 70 | 69 | 53 | 47 |
| run3, px | 116 | 102 | 83 | 78 | 73 | 72 | 70 | 42 | 37 | 37 |

The biggest overview "cloud" is 119 px, about 11 x 11 px. `o240_cloud_count` reads 11, 9 and 7 because its bar is
`CLOUD_MIN_PX` = 50 px - about 7 x 7 px. **Those are specks.** The ceiling of 15 passes a frame with nothing on
it a player can point at.

## Why the man-square cannot serve at O240

T1's `t1_visible_clouds` bar is a man-square: `HEAD_M` = 1.8 m squared in pixels at the bank's distance. At the
overview that yardstick collapses. The O240 sidecar has `cam_y` 264.1 and pitch 25, so the ground at screen
centre is 264.1 / sin(25 deg) = 625 m out and the frame is 2 x 625 x tan(12.5 deg) = 277 m tall over 720 px:
**2.6 px per metre**. A man is 4.7 px tall there, and his square is 22 px - *under* `CLOUD_MIN_PX`. A man-square
bar at this band would count every speck, which is the bar the owner just rejected.

The band's own yardstick is a **tank**: `TANK_M` = 6.0 m, `VehicleGags.RollHull`. A tank is 15.6 px tall at
625 m, a tank-square about 244 px.

## The new measure: `o240_visible_clouds`

Components of the O240 green mask (no `largest_blob` there, as today - the bank is five separated calls) with at
least a **tank-square of pixels at the bank's median distance**, taken from the capture's own camera maths and
never from a hand-picked constant:

1. the median `ground_distance(...)` over the masked pixels, the same ndc mapping `plane_heights` uses, with the
   ground from `ground_of(run_dir, 'gas-o240')`;
2. `frame_h = 2 * med_d * tan(FOV_V_DEG / 2)`;
3. `tank_px = (TANK_M / frame_h * h) ** 2`.

No distance data at all falls back to `CLOUD_MIN_PX`, so the measure is never undefined. `o240_med_d` and
`o240_tank_px` are printed as diagnostics. The counting itself is one shared helper, `count_at_least`, which
`green_shape`'s `t1_visible_clouds` now uses too: **both bands count one way.**

### What it reads on the carried captures

| | run1 | run2 | run3 |
|---|---|---|---|
| `o240_med_d`, m | 542.2 | 541.0 | 541.0 |
| `o240_tank_px` | 323 | 324 | 324 |
| `o240_cloud_count` (50 px) | 11 | 9 | 7 |
| **`o240_visible_clouds`** | **0** | **0** | **0** |

The bank sits nearer than screen centre (542 m, not 625), so the tank-square is a little larger than the 244 px
of the centre-of-frame calculation: 323-324 px. The biggest blob in any of the three frames is 119 px, a third of
a tank. **Nothing at the overview is a cloud.**

No bar is set on `o240_visible_clouds` in part 1: a ceiling or a floor on 0, 0, 0 would bar the broken build.
Part 2 widens the overview card and sweeps it; part 3 bars the count the sweep buys, outside the spread of three
fresh captures.

## What else changed

Nothing. Every other key is byte-identical to the parent's probe on the same directories - checked by diffing
`probe.py --measure` of both probes over `run1`, `run2` and `run3`: only the three new rows appear. All thirteen
bars carry over unmoved and pass.

## Checks run

- `python probe.py --selftest` -> exit 0, cases (a) to (g). Case (g) is the new one: the O240 frame is 277.118 m
  tall at 625 m (hand), a tank-square is 243.0 px (hand), and three patches of 400, 196 and 100 px against that
  bar count **1 of 3**. The sizes and the expected count are picked and read by hand, not from the code.
- `python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1/run{1,2,3}` -> the diff above.
- `python probe.py` -> exit 0, every bar OK, the three O240 frames 341 / 356 / 371 (different, so the frame
  refusal passes).

## Not touched

Every T1 and close-gas measure and bar, `GasPuffBlock`, `GasPuffWide`, `GasPuffStand`, `GasCloudBlock`, and the
owner's open close-gas row in `docs/reference/decisions.md`. This unit only changes what the overview draws.
