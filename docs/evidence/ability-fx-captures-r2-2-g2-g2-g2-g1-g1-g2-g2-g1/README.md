# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1: one bank is one cloud, 2026-10-22

**Parts 1 and 2 of 3.** This folder holds the probe and part 2's density sweep. The look still has not moved -
part 2 measured every density and kept the build's own block 3 (below) - so there are no captures of this build
packed here and `python probe.py` exits 1 with `three runs needed`. Part 3 shoots `run1/`, `run2/`, `run3/` and
`cards/` and sets the ceiling from their spread.

## The owner's complaint

> The close bank is still 38, 41 and 38 clouds. That count was left unbarred because it overlaps the old 39, 44
> and 42, and the fringe specks are what hold it there. Count only a blob a player can see, and put a ceiling on
> that count outside the spread of three captures so one bank is one cloud. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2`'s tip. Bar outside the spread of three captures of
> the current build.

## The finding: it is not the specks

**The count is held by about 22 man-sized, plainly visible masses, not by fringe specks, so the probe alone
cannot reach one cloud - the build must merge.**

Measured on the parent's own packed captures (commit `a7739a66c7`, T1 1280x720, about 240k green pixels). The
components of the T1 green mask, biggest first, run1: 69831, 20554, 18786, 12718, 12688, 11247, 10976, 9948,
9582, 9491, 5746, 5120, 4693, 4547, 4055, 3398, 3319, 2917, 2892, 2859, 2680, 2473, 1815, 1758, 1502, 915, ...
Counted against a size bar:

| bar, px | 50 | 200 | 1000 | 2000 | 5000 | 10000 | 20000 |
|---|---|---|---|---|---|---|---|
| run1 / run2 / run3 | 38/41/38 | 27/30/30 | 25/25/25 | 22/22/22 | 12/12/12 | 7/7/7 | 2/2/2 |

Per blob, the median greenness over the plate is 9-34 and the median luma contrast 0.13-0.69: **there is no faint
family**. The specks are not a separate dim population that a contrast bar could sweep away, and dropping every
speck still leaves about 22 masses. Any honest probe-only change takes 38 to about 23. One cloud is a build
change.

## The new measure: `t1_visible_clouds`

Components of the T1 green mask (before `largest_blob`) whose pixel count is at least a **man-square** at the
bank's median distance. The man-square comes from `measure_t1`'s own camera maths, not from a hand-picked
constant:

    frame_h = 2 * med_d * tan(FOV_V_DEG / 2)      # the frame's height in metres at the bank
    man_px  = (HEAD_M / frame_h * h) ** 2         # a 1.8 m man, squared, in pixels

At these captures `med_d` is 31.7 m, so a man is about 41 px tall and a man-square is **1669 px**
(printed as the diagnostic `t1_man_px`). A blob smaller than the man standing in the gas is not a cloud a
player sees. With no height data - no pixel column reaching the ground - the bar falls back to `CLOUD_MIN_PX`.

`t1_cloud_count` (the flat 50 px bar) stays printed beside it as a diagnostic.

On the parent's captures, measured with this probe:

| run | `t1_man_px` | `t1_visible_clouds` | `t1_cloud_count` (diagnostic) |
|---|---|---|---|
| run1 | 1669 | 24 | 38 |
| run2 | 1668 | 24 | 41 |
| run3 | 1669 | 23 | 38 |

Every other measure is byte-identical to the parent's (`diff` of the two probes' `--measure run1` output shows
only the two new rows).

## Not barred yet

`t1_visible_clouds` has **no bar**. Part 1 moves no look, so there is nothing new to bar, and a measure the
change did not move is never barred in this unit's line. Its ceiling comes from part 3's three captures of the
merged build, set outside their spread - never from a number the probe also computes from the bar. Every other
bar in `probe.py`'s `BAR_VALUES` is the parent's, carried unchanged.

## Part 2: density was tried and it does not buy the merge

Three capture passes of this checkout at `TW_GAS_WAIT_FRAMES` 200 (`GasBankStills`, gas + plate), one per
`CombatFx.GasPuffBlock`, `GasPuffWide` 2.0 / `GasPuffStand` 0.50 throughout. Bars carried from the parent in
brackets.

| `GasPuffBlock` | lane between cards | `t1_visible_clouds` | `t1_cloud_count` | `t1_top_share` [>= 0.28] | `green_low_share` [>= 0.30] | `green_height_frac` [0.08-0.15] | `green_rise_m` [>= 2.4] | `green_soft_share` [>= 0.14] | `green_area_frac` [>= 0.05] | `green_px` |
|---|---|---|---|---|---|---|---|---|---|---|
| 3, KEPT | 12 m | 24 / 24 / 23 | 38 / 41 / 38 | 0.2908 | 0.3045 | 0.0836 | 2.653 | 0.1448 | 0.0532 | ~240k |
| 2 | 8 m | 10 | 18 | 0.8408 | **0.1112** | **0.3100** | 7.559 | **0.0954** | 0.4512 | 494571 |
| 1 | 4 m | **1** | 5 | 0.9991 | **0.0685** | **0.5470** | 10.289 | **0.0411** | 0.8218 | 758028 |

Block 1 draws exactly the one cloud the owner asked for - and it is a green wall over 82% of the frame, with the
gas standing far over a man's head. Block 2 is the same fault, half as far. 2.25x (block 2) and 9x (block 1) the
cards at an unchanged card size doubles and triples the coverage, and the fill sits HIGH on screen, so the share
under a 1.8 m head falls with the block. **Density pulls against the owner's `green_low_share` floor just as
width does, so block 3 is kept**: the close bank is about 23 visible clouds, not one. That measured line is added
under the existing open row in `docs/reference/decisions.md`; the floor is his and is not dropped here.

Untried, and not measured by any capture: block 2 with the width/stand pair re-fitted DOWN to the shorter 8 m lane
(`GasPuffWide` 1.2 / `GasPuffStand` 0.83, so the card is 8.4 m wide and still ~7 m tall). Its capture pass was
refused in this leg's session, so it is left for the next leg to measure, not guessed at here.

`BurstRecipeTests.TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat` is untouched: the block stays 3, so
both of its asserts and their words still describe the build.

## What part 2 planned to try (the plan's own words)

The width knob is **blocked**: every `GasPuffWide` over 2.0 broke the owner's `green_low_share` >= 0.30 floor
(the parent's sweep table), and that floor is the open row in `docs/reference/decisions.md`, "The close gas: one
cloud, or gas that still pools under a man's head?". It is his and it is waiting on him, so part 2 does not
reopen it and does not widen past 2.0. The untried knob is **density**: `CombatFx.GasPuffBlock` 3 -> 2 keeps
every card's size (the width comes from `Thick`/`h2` x `GasPuffWide`, not from the block) and only halves the
12 m lane between cards, so the silhouette's top does not rise - which is what broke the floor - while the lanes
that split the mask fill in.

## Checks run in part 1

- `python probe.py --selftest` -> exit 0, cases (a) to (f); (f) is the new man-square case, three patches of
  144, 64 and 64 px against `man_px` 100 -> `t1_cloud_count` 3, `t1_visible_clouds` 1.
- `python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2/run1` (and run2, run3) -> the
  table above.
