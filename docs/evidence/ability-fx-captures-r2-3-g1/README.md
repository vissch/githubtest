# The chlorine bank, round 3-g1: one cloud, not a field of lily pads

The owner's words (2026-10-20): at T1 the chlorine bank is a field of flat green cards with hard edges and mud
showing in the gaps (`gas-t1-s0`, `gas-t1-s1`, `gas-t1-s2`). It reads as lily pads, not one low cloud. Draw one
soft bank per seed, and measure the ground still showing inside the bank's outline.

## The gap reading

`t1_gap_frac` is new in this round's `probe.py` (copied from `ability-fx-captures-r2-3/probe.py`, which keeps its
three bars unchanged as regression bars below). For a seed's T1 frame:

1. `blob` is r2-2/r2-3's existing cleanup: the single largest coherent patch of pixels the bank changed from its
   no-gas plate.
2. `closed = closing(blob, R)`: dilate `blob` by `R` pixels, then erode by `R` (the same 4-neighbour grow loop,
   run on the complement to erode). A closing joins separate cards across a gap of up to `2*R` pixels while
   keeping the bank's own outer edge - a row/column fill cannot do this, since a regular grid of pads fills to
   exactly the pads and reads a gap of 0.
3. `t1_gap_frac = (closed & ~blob).sum() / closed.sum()`: of the pixels inside the bank's closed outline, the
   share the bank did **not** itself change. Lower is better - 0 is one unbroken cloud, no mud showing inside
   its own silhouette.

`R` is 3 m - half a hair under the 4 m field cell (`MapData.FieldCellSize`) the per-cell cards sit on, so a
one-cell gap between neighbouring cards closes, but a real hole in the bank (wider than the 4 m cell grid) stays
open. 3 m is converted to pixels at the blob's own distance (its bottom-centre pixel, the same camera maths as
`gas_t1_height_m`), then clamped to 6-40 px. Reported alongside it, not barred: `t1_closed_px`, `t1_gap_px`,
`t1_close_radius_px`, and `t1_patch_count` (patches of the raw `changed` mask over 2000 px - "one bank per
seed").

`probe.py --selftest` checks `closing` against two hand calculations worked outside the code: a 20x20 solid
square with a 4x4 hole cut from its middle, closed at R = 3, comes back as the full 400 px square with no hole
(`gap_frac = 16/400 = 0.0400`); the same square with no hole closes to itself (`gap_frac = 0.0`).

## The gap bar

Set the same way r2-3 set its three bars: against the baseline spread of the **unchanged** build (`2ba27d4072`,
this round's starting point), but `t1_gap_frac` is lower-is-better, so the formula runs the other way -
`GAP_BAR = baseline_min - max(0.05, baseline_max - baseline_min)`, a full spread-width *below* the whole spread
(r2-3's bars go a margin *above* theirs, since those are higher-is-better). Never widened afterwards to make a
run green.

| Seed | `t1_gap_frac` (baseline, unchanged build) |
|---|---|
| s0 | 0.6493 |
| s1 | 0.5026 |
| s2 | 0.5498 |

Spread = 0.6493 - 0.5026 = 0.1467. Margin = max(0.05, 0.1467) = 0.1467. **`GAP_BAR` = 0.5026 - 0.1467 =
`0.3559`**.

The unchanged build's own mud-between-the-cards reading is 0.50-0.65: half or more of the bank's own silhouette
is ground the bank did not touch, which is exactly the lily-pad field the owner described. The fix (the next
leg, "one soft bank per seed") has to bring that under 0.36 on every seed, a full spread-width better than the
worst the current build ever showed.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure: r2-3's three bars (regression, unchanged) plus `t1_gap_frac` (new). `--selftest`, `--pack`, `--dir`, `--out` all unchanged in shape |
| `baseline/` + `baseline.json` | three seeds, two bands, of the build **before** this round's fix (`2ba27d4072`, the tip of `ability-fx-captures-r2-3` this round starts from) - the control the gap bar is set against |

The root of this folder holds no stills of its own yet: the "after" pictures (the fix itself, one soft bank per
seed) and the committed root-level 12 stills are the next leg's work. `probe.py` with no arguments will exit 1
until they are captured and packed here.

The harness and commands are unchanged from r2-3 (`GasBankStills.cs`, both passes on the same build, frame counts
only):

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g1/probe.py --pack <dir> --dir baseline --out baseline.json
```
