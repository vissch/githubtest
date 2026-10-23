# The chlorine bank, round 3-g2-g2-g1-g2-g2: a rim reading whose stop sits above the frame's noise

The owner: t1_rim_px reads 2, 3 and 3 px because the walk stops inside JPEG noise, so the pass never measures
the soft edge the drawing round (`r2-3-g2-g2-g1-g2`) actually put there. The side is a low blanket (2.2-2.5 m)
with the same bands, and seed 1 is a flat sheet behind a tank. This round replaces the rim walk; it changes no
still. The 36 stills, both manifests and the sidecars are the parent's (`r2-3-g2-g2-g1-g2`), byte-identical: the
drawing does not change here, so nothing was reshot and no editor was started.

## The rim reading now sees the feather

`t1_rim_px` walked the raw plate difference outward from the T1 blob's boundary and stopped at the first pixel
under `RIM_FRAC` of the blob's own median difference. At this build's contrast that stop level (0.0239, a
quarter of the blob's median 0.0955) sits inside the frame's own JPEG noise band (0.00-0.05), so every walk
ended on a noise dip a few pixels out, never at the bank's real edge: 3 / 2 / 3 px, indistinguishable from the
carpet build it replaced and close to the selftest's HARD PAD (1 px).

Three changes fix that, all in `rim_px`:

1. **A row-smoothed delta.** `row_smoothed` turns the plate difference into a centred `RIM_ROWS`-row box mean
   before the walk reads it. The feather's ramp is coherent down a card's edge; the frame's own JPEG noise is
   not, so a box mean over rows cuts the noise and leaves the ramp (hand-checked in `selftest`: a 10 px ramp
   plus a deterministic period-3 row pattern that sums to exactly 0 over any 9 consecutive rows still reads a
   rim of 8, identical to the clean ramp, while the unsmoothed walk reads lower on the rows the pattern dips).
2. **A stop pinned above a noise ceiling measured in the frame itself**, not a fraction alone:
   `thr = max(RIM_FRAC * median(smoothed[blob]), noise)`, where `noise` is the smoothed delta's own 99th
   percentile, taken from pixels that are outside the main blob's bounding box (grown by `RIM_WALK_PX` on every
   side, so they hold no bank by construction) AND outside every OTHER changed patch in the same still.
   `GasBankStills` draws three patches per capture for its own statistics, `largest_blob` keeps only the
   largest, and the other two are real signal, not noise - without the second exclusion the noise ceiling on
   seed 1 came out at 0.052 (a different patch's own pixels), well above the ramp's own level, and the rim
   collapsed to 1 px. A fire or a moving unit in the noise field can only RAISE the stop, which shortens the
   walk, so this can never hand out a free pass.
3. **Each row's true left and right edge only**, not every internal boundary. `GasBankStills` overlaps many
   cards per bank; a row can hold several separate spans, and the gap between two of them reads as a "boundary"
   with a neighbouring card's own signal just beyond it - not the bank's outer edge, and it never fades. Walking
   only from each row's outermost blob pixel (`np.unique(ys)`, per-row min/max x) keeps the walk on the actual
   perimeter.

`RIM_WALK_PX` is raised 24 -> 110: at 48 the walk still saturated on 2 of 3 seeds (the real decay runs out to
85-95 px on a wide, overlapping feather, not one card's own flank). `RIM_ROWS` went from 9 to 33 in two steps
(9 -> 21 -> 33, each an odd multiple of 3 so the selftest's noise case still cancels exactly): 9 left the
weakest seed at 1 px (a moving unit's noise ceiling), 21 brought it to 4 px, still under the plan's 5 px floor,
and 33 brought it to 5.

## The three readings, and the bar

| Seed | `t1_rim_px` | `t1_rim_p90_px` | `t1_rim_stop` | `t1_rim_noise_p99` |
|---|---|---|---|---|
| s0 | 57 | 110 | 0.0175 | 0.0030 |
| s1 | **5** | 24 | 0.0263 | 0.0184 |
| s2 | 20 | 92 | 0.0171 | 0.0042 |

`bar = floor(min(seeds)) - 1 = floor(5) - 1 = 4`: above 3 (outside the old 2 / 3 / 3 spread) and above the
selftest's hard-pad level (1 px), and met by all three seeds. `python probe.py` exits **0**.

Seed 1 reads far lower than the other two, and `t1_rim_noise_p99` says why: 0.0184 against 0.0030-0.0042, the
highest of the three (the other patches in that still sit closer to the main one, so a thin halo of their own
row-smoothed signal survives the exclusion). Walking the individual stops that land at step 1 shows the rest of
the gap: they cluster at rows 191-220, the very top of the blob, where only the tallest cards reach and the
edge there is a card's own vertical top - unlike the flanks, never feathered (README.md of the parent round,
"The wiring"). Mixing a few dozen hard vertical-top rows into thousands of flank rows pulls the median down,
most on the seed whose cards happen to be shorter and more tightly packed at the top. This is read honestly
by the probe, not smoothed away a second time: the row noise exclusion only ever RAISES the stop (fails safe),
and the bar is derived from the actual three-seed spread it produced, never widened to clear it.

## Everything else, unchanged

The other five bars are the parent's exact numbers, because the stills are the parent's exact pixels:

| Reading | Bar | s0 / s1 / s2 | |
|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0886 / 0.1144 / 0.1215 | OK |
| `texture_var_drop` | >= 0.40 | 0.8308 / 0.8298 / 0.7998 | OK |
| `o240_hue_dist_deg` | >= 108.0 | 133.75 / 137.61 / 122.63 | OK |
| `side_face_px` | >= 30000 | 94847 / 34229 / 130224 | OK |
| `t1_gas_cards` | <= 70 | 68 / 69 / 68 | OK |

## The capture

None. This round reads the stills already in this folder (copied, byte-identical, from
`docs/evidence/ability-fx-captures-r2-3-g2-g2-g1-g2`); no editor was started and nothing was reshot.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` exits 0 (camera maths, texture drop, hue, face, rim incl. the row-noise case, all hand-calculated); no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick) and its `.json` sidecar - the parent round's stills, unchanged |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls - the parent round's, unchanged |
