# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1: one bank is one cloud, 2026-10-22

**Part 1 of 3.** This folder holds the probe only. No look has moved yet, so there are no captures of this
build here and `python probe.py` exits 1 with `three runs needed` - by design. Part 2 merges the bank in the
build, part 3 shoots `run1/`, `run2/`, `run3/` and `cards/` and sets the ceiling from their spread.

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

## What part 2 will try (not done here)

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
