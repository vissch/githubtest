# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1: one bank is one cloud, 2026-10-22

**All three parts, done.** `python probe.py` exits 0 on `run1/`, `run2/`, `run3/` here. The look did not move:
every knob that merges the bank - width (the parent) and density (part 2) and the last re-fitted candidate
(part 3) - breaks the owner's `green_low_share` >= 0.30 floor, so `GasPuffBlock` stays 3 and the count is barred
where the build stands. **The close bank is ~24 clouds a player can see, not one.** The reason is the floor, and
it is his open row in `docs/reference/decisions.md`, "The close gas: one cloud, or gas that still pools under a
man's head?".

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

## Part 3: the three captures, and the ceiling

Three fresh captures of this build (`GasPuffBlock` 3, `GasPuffWide` 2.0, `GasPuffStand` 0.50), `TW_GAS_WAIT_FRAMES`
200 / 215 / 230, two batch passes each into one directory (the second with `TW_GAS_PLATE=1`), packed into `run1/`,
`run2/`, `run3/`; `cards/` is one `GasBankCards` launch.

### The last candidate, measured and refused

Part 2 left one untried row: block 2 with the card re-fitted DOWN to its shorter 8 m lane, so it just touches its
neighbour instead of overlapping it. Lowering the width is not the owner's barred direction (only widening past
2.0 is), so it was shot the same way, one capture at wait 200.

| | `t1_visible_clouds` | `t1_cloud_count` | `green_low_share` [>= 0.30] | `green_height_frac` [0.08-0.15] | `t1_top_share` | `green_px` |
|---|---|---|---|---|---|---|
| block 3 / wide 2.0 / stand 0.50, KEPT | 24 | 38 | 0.3045 | 0.0836 | 0.2909 | 240081 |
| block 2 / wide 1.2 / stand 0.83 | 24 | 48 | **0.1938** | **0.1539** | 0.2949 | 321968 |

It merges nothing - the same 24 visible clouds - and it still breaks the floor AND the height ceiling. Refused.
With width, density and this re-fit all measured, no knob merges the bank while the 0.30 floor holds.

### The bars on these three captures

| bar | run1 / run2 / run3 | spread | bar | margin |
|---|---|---|---|---|
| `t1_cloud_ceiling` (`t1_visible_clouds`) | 24 / 24 / 23 | 1 | `lt` 26 | 2 |
| `t1_top_share` | 0.2909 / 0.2910 / 0.2901 | 0.0009 | `ge` 0.28 | 0.0101 |
| `green_low_share` | 0.3045 / 0.3048 / 0.3034 | 0.0014 | `ge` 0.30 | 0.0034 |
| `green_height_frac` | 0.0836 / 0.0833 / 0.0841 | 0.0008 | `le` 0.15 | 0.0659 |
| `green_height_floor` | the same | 0.0008 | `ge` 0.08 | 0.0033 |
| `green_rise_m` | 2.653 / 2.643 / 2.669 | 0.026 | `ge` 2.4 | 0.243 |
| `green_area_frac` | 0.07577 / 0.07591 / 0.07606 | 0.00029 | `ge` 0.05 | 0.02577 |
| `green_soft_share` | 0.1639 / 0.1645 / 0.1637 | 0.0008 | `ge` 0.14 | 0.0237 |
| `o240_cloud_count` | 11 / 9 / 7 | 4 | `ge` 3 | 4 |
| `o240_cloud_ceiling` | 11 / 9 / 7 | 4 | `lt` 15 | 4 |
| `o240_cloud_share` | 0.4777 / 0.4384 / 0.3676 | 0.1101 | `ge` 0.25 | 0.1176 |
| `o240_green_area_frac` | 0.0010 / 0.0008 / 0.0006 | 0.0004 | `ge` 0.0001 | 0.0005 |
| `pose_error_max` | 0 / 0 / 0 | 0 | `lt` 0.50 | 0.50 |

Every margin is at or over the spread. `t1_cloud_ceiling` is the one new bar, and it is honest about one thing the
plan hoped for and the captures denied: it does **not** separate this build from the parent's, because the look did
not move - the parent reads the same 24 / 24 / 23 (part 1's table). It pins the count where three captures put it,
and it fails the day a change lets the bank fray back out. `t1_cloud_count` (38 / 41 / 38, the flat 50 px bar) is
still barred on nothing: part 1 showed the specks are not what holds the count.

### Cards and draws

T1, from `run1/manifest.json`: `gas_cards` 106 (of `FlipbookFx.MaxCards` 1536) and `draw_calls` 133. The O240
overview draws 10 cards and 118 calls. Block 2 would have been ~2.25x the cards; it was refused on the look, not
the cost, and the kept build sits at 7% of the card budget.

### The bank is ~24 clouds, not one

Plainly: the owner asked for one cloud, and this build draws about 24 that a man standing in the gas would not
hide. Block 1 draws exactly one - a green wall over 82% of the frame (part 2) - and every merge breaks his
`green_low_share` >= 0.30 floor. The floor is his, so it was not dropped. The measured line is under the existing
open row in `docs/reference/decisions.md`; option (b) there is the only path to one cloud.

### Checks run in part 3

- `python probe.py --selftest` -> exit 0, cases (a) to (f).
- `python probe.py` -> exit 0, the table above, `t1_cloud_ceiling` OK.
- `python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2/run1` -> `t1_visible_clouds` 24,
  `t1_cloud_count` 38 (run2 24 / 41, run3 23 / 38), unchanged by the new bar.
- `gate.ps1 -EditOnly` -> exit 0.
