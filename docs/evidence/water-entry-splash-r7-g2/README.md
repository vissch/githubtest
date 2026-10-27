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
| `crown_top_px_post` | <= 10 | the crown has COME DOWN: the topmost row above the mark's row, in pixels, where the splash still changes the crown box at +0.3 s |
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

## The fix, captured three times

Not yet: this folder is committed with the baseline only, before any behaviour changed.
