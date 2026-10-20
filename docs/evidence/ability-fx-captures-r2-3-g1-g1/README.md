# The chlorine bank, round 3-g1-g1: count the ground inside the outline, with no close

The owner's words (2026-10-21): at zoom 30 the T1 bank is still rows of flat green cards with hard edges and mud
in the holes (`gas-t1-s0`, `gas-t1-s1`, `gas-t1-s2`). `gas_cards` is 1047, 904 and 747. The gap close is 40 px, so
`t1_gap_frac` 0.245-0.317 slips under the bar 0.3559 while the pads still show. Draw one soft bank per seed, and
count the ground inside the outline with no close that joins cards.

## Why the old gap reading let the pads through

r2-3-g1's `t1_gap_frac` closed the blob first: dilate by `R`, then erode by `R`. `R` is 3 m at the blob's own
distance, and at zoom 30 that converts to the clamp's ceiling, **40 px**. A dilation of 40 px bridges a gap of up
to 80 px, which is far wider than the gap between neighbouring cards on the 4 m field grid - so the closing joined
the pads into one slab, and the "gap inside the outline" it then measured was only the slab's own ragged rim. The
number fell under the bar while the picture did not change.

## The new reading: `t1_hull_gap_frac`

For a seed's T1 frame:

1. `blob` - unchanged from r2-2/r2-3: the single largest coherent patch of pixels the bank changed from its no-gas
   plate.
2. `hull = convex_hull_fill(blob)` - the smallest filled convex outline that contains the blob. **No dilation
   anywhere**, so the blob is never grown and two separate cards are never joined. Only the per-row leftmost and
   rightmost pixels can be hull vertices (any other pixel of a row sits strictly between two pixels of that row),
   so those are the only candidates; a monotone chain over them gives the hull polygon, and the fill is a scanline
   taking the min and max x of the polygon's edges at each row.
3. `t1_hull_gap_frac = (hull & ~blob).sum() / hull.sum()` - of the pixels inside the bank's own outline, the share
   the bank did **not** change. Lower is better. A new name, so it is never read as the old closed number.

Reported alongside it as rows, not bars: `t1_hull_px`, `t1_hull_gap_px`, `gas_cards` (from the pass manifest's row
for that still), and the old `t1_closed_gap_frac` with `t1_closed_px`, `t1_closed_gap_px` and
`gap_close_radius_px` - `closing` and its radius are kept, moved out of the bars into the rows, so the two
measures stand side by side in one json and the old one can never carry a claim again.

`probe.py --selftest` checks all three against hand calculations worked outside the code:

| Case | hull | blob | gap | `gap_frac` |
|---|---|---|---|---|
| (a) 20x20 solid square, a 4x4 hole cut from its middle | 400 px | 384 px | 16 px | `0.0400` |
| (b) the same square, no hole | 400 px | 400 px | 0 px | `0.0` |
| (c) **the pad case**: a 3x3 grid of 2x2 pads, starts at 0, 3 and 6, extent 8x8 | 64 px | 36 px | 28 px | `0.4375` |

Case (c) is the one the old closing missed (its dilation bridges the gaps and reads about 0) and the one a
row/column fill reads as 0. The two `closing` cases r2-3-g1's selftest had are kept unchanged.

## The bar

Set the same way r2-3-g1 set its gap bar, against the baseline spread of the **unchanged** build (`f427286f0d`,
this round's starting point). `t1_hull_gap_frac` is lower-is-better, so the formula runs the other way:
`HULL_GAP_BAR = baseline_min - max(0.05, baseline_max - baseline_min)`, a full spread-width *below* the whole
spread.

| Seed | `t1_hull_gap_frac` | `t1_closed_gap_frac` (the old reading) | `gas_cards` |
|---|---|---|---|
| s0 | 0.3206 | 0.3062 | 1047 |
| s1 | 0.3702 | 0.3167 | 904 |
| s2 | 0.2691 | 0.2454 | 747 |

Spread = 0.3702 - 0.2691 = 0.1011. Margin = max(0.05, 0.1011) = 0.1011. **`HULL_GAP_BAR` = 0.2691 - 0.1011 =
`0.1680`**.

The capture reproduces the owner's frames exactly: `gas_cards` 1047 / 904 / 747 and `t1_closed_gap_frac` 0.3062 /
0.3167 / 0.2454 are r2-3-g1's own after-numbers to four places, so this baseline is the same build he looked at,
frame for frame. With no close, a quarter to over a third of the bank's own outline is ground the bank never
changed - the mud in the holes, now counted where the closing hid it. The fix (the next leg, "one soft bank per
seed") has to bring that to 0.1680 or under on every seed.

The other three bars already pass on this baseline (`t1_changed_frac` worst 0.5488 against 0.08,
`texture_var_drop` worst 0.6950 against 0.40, `o240_hue_dist_deg` worst 116.37 against 38.0), so they stand as
regression bars: the soft bank must not lose them. `python probe.py --dir baseline --out baseline.json` exits 1,
because the hull gap bar is the point of this round and the baseline is the thing it is set from.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure: r2-3's three bars (regression, unchanged) plus `t1_hull_gap_frac` (new). `--selftest`, `--pack`, `--dir`, `--out` all unchanged in shape |
| `baseline/` + `baseline.json` | three seeds, two bands, of the build **before** this round's fix (`f427286f0d`, the tip of `ability-fx-captures-r2-3-g1` this round starts from) - the control the hull gap bar is set against |

The root of this folder holds no stills of its own yet: the "after" pictures (one soft bank per seed) are the next
leg's work, and `probe.py` with no arguments exits 1 until they are captured and packed here.

The harness and commands are unchanged from r2-3-g1 (`GasBankStills.cs`, both passes on the same build, frame
counts only; `Unity.exe` is the editor itself, `C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe`,
not the `unity` CLI):

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g1-g1/probe.py --pack <dir> --dir baseline --out baseline.json
```
