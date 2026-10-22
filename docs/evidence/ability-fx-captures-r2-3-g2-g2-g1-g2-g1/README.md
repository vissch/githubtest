# One bank, one silhouette — round 3-g2-g2-g1-g2-g1: the seam count

The owner (2026-10-22): "The close shots are still a stack of horizontal green bands (gas-t1-s0, gas-t1-s1 and
gas-t1-s2), and the far shot is the same stripes. Draw one bank that shares one silhouette, and bar a seam count
outside this striped build."

This folder is the evidence of that round. It starts with the reading and its bar; the drawing change and the
captures of the new build follow in the same folder.

## What the stripes are

Each standing 4 m cell draws one `Upright | Anchored` card whose ink sits in the bottom third, so the bank is
~66 short ink bands at ~66 different depths. At T1 (pitch 25, camera 31.5 m over the ground) one grid row is
2.57 m of depth = ~24 screen px, and the ink band does not bridge it: the ground shows between the rows. Nothing
in the drawing ties the cards into one shape.

## The reading: `t1_seam_runs`

For every column of the T1 blob holding at least `MIN_COLUMN_PX` (8) masked pixels, close the gaps of
`SEAM_CLOSE_PX` (4) pixels or fewer, count the runs that are left, and take the median over those columns. A gap
of 4 px at T1 is 0.2 m of ground — under the JPEG block and the card's own alpha dither; a real seam shows ground
over several tenths of a metre. One bank with one silhouette is 1-2 runs; a stack of cards is many.

`o240_seam_runs` is the same reading on the overview blob. It is **reported, not barred**: the owner asked for one
seam count, and the same drawing change moves both.

Three hand-worked cases are in `probe.py --selftest`: a 720x40 mask of 9 ink bands 10 px tall with 14 px gaps
reads 9 (no gap closes); the same mask with 3 px gaps reads 1 (every gap closes); one solid 100 px band reads 1.

## The striped build, measured

The striped build is commit `8d27215f9e` — the tip this lane started from. Its three committed T1 stills and
their plates, measured by this folder's `probe.py` (`--dir ../ability-fx-captures-r2-3-g2-g2-g1-g2`, output
committed here as `striped.json`):

| Reading | s0 | s1 | s2 |
|---|---|---|---|
| `t1_seam_runs` | 7 | 6 | 7 |
| `o240_seam_runs` | 1 | 1 | 1 |

That run exits **1** with `t1_seam_runs want <= 3  worst 7.0  MISSED`, which is the point: the bar refuses the
build the owner complained about.

(The plan for this round quoted 9 / 8 / 9 from an ad-hoc measurement made on the same stills before `seam_runs`
existed. `seam_runs` counts on the blob `measure_t1` already builds, whose 1 px dilation bridges the thinnest
gaps, so it reads 7 / 6 / 7 on exactly those frames. The bar below is derived from what the committed code
reads, not from the plan's number.)

## The bar

The folder's rule: a bar sits **outside** the spread of three captures of the build it guards, and is never
widened to make a run green.

- **`t1_seam_runs` (`<=`)**: `bar = floor(min(striped) / 2) = floor(6 / 2) = **3**` — half the striped build's
  best seed, and three full spreads (spread 1) below its minimum, so it sits outside the striped spread, while
  one bank with one silhouette (1-2 runs) clears it with room. Rounded down. It has no owner bar.
- Stripes are MORE runs, so like `t1_gas_cards` it is an upper limit (`BAR_DIR` `'<='`), and it is last in
  `BAR_ORDER`.
- Every other bar is carried over from `ability-fx-captures-r2-3-g2-g2-g1-g2` unchanged; see that folder's
  README for their derivations.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` exits 0; no arguments measures the stills beside it |
| `striped.json` | this probe's readings of the striped build's committed stills (exit 1), the bar's provenance |
