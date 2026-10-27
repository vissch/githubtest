# water-entry-splash-r7-g2 — the evidence, and what it proves

The owner looked at r7's water entry and overturned two of its own answers:

> The ring is a faint disc and the crown does not come down. `land-noring.png` keeps the column and only loses the
> disc, and `post.png` at +0.3 s still holds a white sprite in the water.

So this round has its own folder, its own probe and its own bars. r7's folder, its numbers and its README are left
exactly as they are.

| key | bar | what it means |
|---|---|---|
| `ring_step` | >= 0.08 | the ring's value step on the water: p90 of the water band minus p90 of the same band with the ring parked; the **min** of land + 0.05 s and land + 0.3 s |
| `ring_px_band` | >= 1000 | pixels of the band (1650 px) the ring alone moves past 0.02 luma; the **min** of the two moments |
| `ring_hollow` | <= 0.5 | the ring opens from a HOLE, not a point: mean ring-only lift inside 0.45 of the crest radius over the mean on the crest itself, at +0.3 s |
| `crown_top_px_post` | <= 10 | the crown has COME DOWN: the topmost row above the mark's row, in pixels, where the CARDS still change the crown box at +0.3 s (the ring parked in both halves of the pair) |
| `crown_contrast_land` | >= 0.15 | r7's key, unchanged — a **guard**, so the fix cannot reach the other four by deleting the crown |

The first four are **regression bars**: the current build is on the failing side of each, proved over three
captures below. `crown_contrast_land` is a **guard**: the current build already clears it at 0.275, and it is kept
so the crown must still STAND at the landing frame and be GONE a third of a second later.

**What changed from r7's measures, and why.** r7 measured the ring as `ring_only_px`, a COUNT of pixels the ring
alone changed anywhere in a 6 m window that also holds the standing crown — a faint disc wide enough passes that.
Here the ring is a VALUE STEP inside its own footprint: the **water band**, columns `mark_px_x` +/- 2.2 m and rows
from 0.3 m of height above the mark's row down to 1.0 m below, with the crown's own column (+/- 0.5 m) **cut out**,
so the crown can never pay for the ring. And r7 barred the crown to still READ at +0.3 s
(`crown_contrast_post >= 0.09`, cleared at 0.208) — which is exactly the white sprite the owner now objects to. The
owner's latest word wins, so the post frame now bars the crown DOWN. `crown_contrast_post` is still reported, for
the record, but nothing passes or fails on it.

## How to read this folder

`probe.py` measures the stills and writes `probe.json`; every number there is measured, never typed. Its own
docstring is the long account: the camera, the three regions, the threshold, and why each moment has its own
`-nosplash` reference instead of a plate taken earlier.

```bash
python docs/evidence/water-entry-splash-r7-g2/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
python docs/evidence/water-entry-splash-r7-g2/probe.py --fixed run-4 run-5 run-6   # the fix
python docs/evidence/water-entry-splash-r7-g2/probe.py                             # re-measures both sets from
                                                                                   # probe.json, writes nothing,
                                                                                   # exits 0 only on a full pass
```

- `run-N/` — one capture run, staged by `trench-warfare-3d/AgentScripts/water_entry_fling.cs` and rendered by
  `CaptureRig` in Play in GreyboxCorridor at night, T1 (zoom 30, yaw 21, pitch 25), 1600 x 900, rain 0, both clocks
  pinned, the sim frozen, `pose_error_m` 0, the flight bisected to exactly 43 frames. Nine stills with a `.json`
  sidecar each, plus `timings.txt` (the achieved game-time delta of every frozen moment) and `stage.log`.
- `probe.json` — the baseline block (`*_base_best`, `n_base_captures`, `base_runs`) and, once the fix is captured,
  the fixed block, with a `runs` block holding every single run's own numbers so the spread is visible.

Heights are in PIXELS. `mark_px_per_m` turns ground metres into pixels, but a crown stands UP out of the water and
its height is not a distance along the ground, so `crown_top_px_post` is a pixel count and the probe ASSERTS the
camera is r7's exact projection (`mark_px_per_m` 19.22227) before it measures anything. 53 px is 2.76 m; the metres
are reported for the reader only.

## The baseline: the CURRENT build, captured three times before anything changed

Each run in its OWN editor session (r7's lesson: two runs in one Play session measured `noise_px` ~1030 against 2).
For a `>=` bar the baseline is taken as its **max** over the three and for a `<=` bar as its **min** — its best
showing either way — and the fixed build will be taken the other way round, at its worst.

| key | run-1 | run-2 | run-3 | `_base_best` | bar |
|---|---|---|---|---|---|
| `ring_step` | 0.0630 | 0.0578 | 0.0578 | 0.0630 | >= 0.08 |
| `ring_px_band` | 704 | 782 | 782 | 782 | >= 1000 |
| `ring_hollow` | 0.752 | 0.745 | 0.745 | 0.745 | <= 0.5 |
| `crown_top_px_post` | 53 | 53 | 53 | 53 | <= 10 |
| `crown_contrast_land` (guard) | 0.2968 | 0.2754 | 0.2754 | 0.2968 | >= 0.15 |

`probe.py --base run-1 run-2 run-3` exits 0: all three runs are on the failing side of all four regression bars.
The three agree closely — `ring_step` within 0.005, `ring_px_band` within 78 px, `crown_top_px_post` to the pixel —
so the gap to the bars is the effect's, not the capture's.

**What the baseline says in words.** The ring's own step on the water is 0.058 and it covers 782 of the band's 1650
px: faint, and under half the footprint — the owner's "faint disc". Its lift inside the middle is 0.745 of its lift
on the crest, so it reads as a filled disc and not a ring opening from a hole. And the splash still changes the
crown box 53 px (2.76 m) above the water at +0.3 s: the white sprite that has not come down.

**One bar was made HARDER than the plan asked.** The plan set `ring_px_band >= 600`, and the baseline MEASURED 704,
782 and 782 — the faint disc already covers that much of the band, so 600 would have been cleared by the build the
owner is complaining about. The bar is 1000, 1.28x the baseline's best showing and outside its spread. No bar was
softened.

`crown_top_px_post` is measured from `post-noring.png` against `post-nosplash.png` - the ring PARKED in both
halves, so only the cards count. The pair started as `post.png` against `post-nosplash.png` and that was wrong: a
ring lies flat on the water and its far arc shows ABOVE the mark's row on screen, about 12 px up for a 1.4 m ring
and more once the crest was widened, so the RING paid the crown's bar. It read 22 px on a build whose cards were
already gone. The baseline is re-measured from its own committed stills the same way and is unchanged at 53 px, so
the bar still sits far outside the baseline's spread. The stills themselves were never re-shot.

## The fix, captured three times

`run-4`, `run-5` and `run-6`, each staged in a Play session of its OWN batch editor. Every key is taken at the
build's WORST over the three.

| key | run-4 | run-5 | run-6 | taken | `_base_best` | bar |
|---|---|---|---|---|---|---|
| `ring_step` | 0.1118 | 0.1126 | 0.1127 | 0.1118 | 0.0630 | >= 0.08 |
| `ring_px_band` | 1121 | 1122 | 1124 | 1121 | 782 | >= 1000 |
| `ring_hollow` | 0.481 | 0.475 | 0.480 | 0.481 | 0.745 | <= 0.5 |
| `crown_top_px_post` | 0 | 0 | 0 | 0 | 53 | <= 10 |
| `crown_contrast_land` (guard) | 0.3058 | 0.3049 | 0.3077 | 0.3049 | 0.2968 | >= 0.15 |

`probe.py --fixed run-4 run-5 run-6` exits 0, and so does `probe.py` with no arguments, which re-measures both sets
from `probe.json` and re-checks every bar. r6's carried guards hold too: `noise_px` 2, `ring_px_pre` 0,
`ring_px_land` 3993 against a floor of 1000, `ring_px_post` 3529 against 600, the ring's centroid 0.079 m off its
mark. The three runs agree to 0.001 on `ring_step` and to 3 px on `ring_px_band`, so the gap to the bars is the
effect's and not the capture's.

**What changed in the build.** Three things, and nothing else:

- `WaterEntry.CardLife` (0.7 s for all three cards) is split into `CrownLife` 0.26 s and `WingLife` 0.29 s, each
  passed to its own `books.Add`. The crown goes first, the wings a frame or two behind it, and by the post frame
  the read on the water is the ring's alone.
- `TWRings` in `TWWater.hlsl` draws ONE crest, not two. The second line at `radius * 0.55` is gone; what is left
  is a crest and the dark trough at `radius * 0.78`.
- The crest's profile is widened (full strength to 0.9 of `width`, gone by 1.8 of it, where it was 0.6 and 1.0)
  and `TW_RING_LIFT` goes from 0.26 to 0.45. `width` itself is untouched, because an 8 m shell ring shares it.

**What the pixels overturned this round.**

*The wings were the white sprite, not the crown.* The first fixed build set `CrownLife` 0.30 s - the spec moment
itself - and left the wings at 0.45 s, reasoning that the collapse should be staggered. The capture came back with
`crown_top_px_post` 27 px: 1.41 m, which is a 1.32 m wing standing on the water 1.26 m out, well inside the crown
box. A wing still up IS the thing the owner is objecting to, so the wings come down by the post frame too.

*A batch editor does not reimport an edited file by itself.* The second tuning pass measured numbers identical to
the first to three decimals - the same `ring_crest_r_m`, the same `ring_crest_lift` - because stopping and
restarting Play reloads the C# domain but never re-imported the changed shader or scripts. Every committed run was
shot from an editor LAUNCHED after the edit, which is also what the one-run-one-session rule asks for.

*1.5 was not enough crest.* With the crest reaching 1.5 of `width`, `ring_px_band` measured 965 against the bar of
1000, on the landing frame where the ring is still only 0.96 m across. 1.8 cleared it at 1121.

**The ring is shared, so the shell ring was looked at.** `TWWater.hlsl` serves shell bursts and wading men as well
as the entry, and the plan's risk was that a wider, brighter crest would glare on an 8 m shell ring. One was
opened on the ford and shot with the ring in and parked: its peak lift is 0.172 luma, its brightest pixel 0.621,
and not one pixel of the frame is saturated. It reads as a clean wide ring on the water, not a glare.
