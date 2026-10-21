# The chlorine bank, round 3-g2-g1: one soft bank with a side, not a carpet of cards

The owner (2026-10-21): "At zoom 16 and pitch 8 the side shots are a carpet of flat green cards, not a bank with
a side. `gas-side-s0` and `gas-side-s2` are pads on the mud, and `gas-side-s1` is a stain around the tank's
tracks. The plates are the same frames with the green gone. Draw one soft bank whose upright face a player can
see at that pose, and shoot it three times. Bar outside the spread of three captures of the current build."

## Why the carpet was not a bank

At the side pose every 4 m gas cell drew its OWN card and each 2x2 block drew an 11 m one on top, all jittered up
to 2.4 m and packed at opacity 0.95-1.0. Edge-on, that is dozens of near-opaque quads lying flat on the mud: the
plate-subtracted blob covered 0.09-0.33 of the frame (`side_blob_frac`, measured on g2's own stills with this
probe), and its top edge turned over in 11 rows on seed 0 - a card boundary, not a fade.

A height alone could not catch that. `gas_side_height_m` read 2.80-2.89 m on the carpet, because pads lying out
across the field still have a silhouette top. Two more readings were needed, and they are the new part of this
probe.

## What changed in the drawing

`CombatFx.Banks.cs`:

- **One bank per 2x2 block, not one card per cell.** A cell now only says which block stands and how thick it is;
  the block draws a single 12 m **body** card.
- **A crest.** A second, narrower (0.45 of the body) and fainter card lifted 0.35 m off the ground, boiling out of
  step with the body, so the bank has a top that is not the body's own edge.
- **Jitter 2.4 m -> 1.2 m**, so the blocks line up into one mass instead of scattering.
- **Opacity `GasBankThin` 0.55, `GasBankFull` 0.75** (was 0.95-1.0). These feed `Pack`'s OPACITY, not its fade:
  `GasBankFade` names the fade invariant and is untouched, so the shader's erode threshold still clears and the
  mud and the far field read THROUGH the bank.

The crest is 0.45 of the body, not the 0.72 first tried: at 0.72 its ink topped out at 2.51 m, over a 1.8 m man
(L07); at 0.45 it is 1.70 m. `BurstRecipeTests.TheGasBankCrestStaysUnderAMansHead` holds that, and
`TheGasBankSurvivesTheErodeThreshold` was restated on `GasBankFade` (it had asserted on the wrong quantity,
reading `GasBankThin` as the card's fade).

## The pose (unchanged from g2)

Side band: zoom 16, pitch 8 (`TacticalCamera.PitchMin`), `aimY` raised to `ground + 2 m`. Distance 41.7 m, camera
7.8 m up, 41.3 m out, so 10 m of ground depth beyond the bank's base reads as only 1.5 m of false height against
T1's 3.5 m. A pose guard fails the probe (exit 1, never a silent pass) if a side sidecar's `cam_pitch` is not
within 1 deg of 8 or its camera does not stand within 1.5 m of 7.8 m above the ground.

## The two new readings

Both come off the SAME plate-subtracted `largest_blob` as `gas_side_height_m`, so one blob answers all three.

- **`side_blob_frac`** - blob pixels / frame pixels. A **ceiling**: one bank has a limited footprint, a carpet
  does not. Worked by hand in `selftest()`: a 40-wide, 20-tall blob in a 100x100 frame is 800 / 10000 = **0.0800**.
- **`side_edge_soft_px`** - per blob column, the rows over which the plate difference climbs from 20% to 80% of
  that column's interior (the median difference over the lower half of its masked rows); the median over columns.
  A **floor**: a hard card edge turns over in one or two rows. The fade itself sits below `LUMA_BAR`, so it is
  OUTSIDE the blob - the scan window is `EDGE_LOOK` (48) rows above the column's top masked row down to its
  bottom one, read on the raw difference. Worked by hand in `selftest()`: a column whose difference ramps 0.05,
  0.10 ... 0.50 over 10 rows and then holds 0.50 has an interior of 0.50; 20% of it (0.10) is first reached at
  row 1 and 80% (0.40) at row 7, so the span is **6** rows - the 20%-to-80% part of a 10-row ramp.

Neither expected value is taken from a run of the code it checks.

## The bars

The g2 rule, re-run against the three captures of THIS build: a floor is `min(seeds) - margin` rounded DOWN, a
ceiling is `max(seeds) + margin` rounded UP, so rounding never moves a bar inside the spread.

| Reading | Seeds (s0 / s1 / s2) | Min / max | Margin | Bar |
|---|---|---|---|---|
| `gas_side_height_m` | 2.154 / 2.158 / 2.527 | min 2.154 | 0.5 m | **>= 1.6** (was 2.2) |
| `side_edge_soft_px` | 28 / 46 / 22 | min 22 | 1 row | **>= 21** (new) |
| `side_blob_frac` | 0.1284 / 0.0477 / 0.2187 | max 0.2187 | 0.03 | **<= 0.25** (new, a ceiling) |

`t1_changed_frac` (>= 0.08), `texture_var_drop` (>= 0.40) and `o240_hue_dist_deg` (>= 69.8) are g2's and are NOT
re-set here: a bar is never lowered to make a run green. The same three readings on g2's own stills, for the
comparison: `side_blob_frac` 0.2286 / 0.0902 / 0.3293 and `side_edge_soft_px` 11 / 30 / 22 - the new build's
footprint is smaller on every seed and its edge is softer on two of three.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths, the texture/hue readings, `footprint_bias_m` and both new side readings against hand calculations; no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars and the ceilings |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick, gas calls skipped) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |

Shot on the tip of `lane/show/ability-fx-captures-r2-3-g2-g1`, commit `73e7ed7694`. Both passes, same build:

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2-g1/probe.py --pack <dir>
```

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 | |
|---|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0958 | 0.1088 | 0.1156 | OK |
| `texture_var_drop` | >= 0.40 | 0.6096 | 0.6277 | 0.6753 | OK |
| `o240_hue_dist_deg` | >= 69.8 | 71.67 | 88.75 | 37.15 | **MISSED on s2** |
| `gas_side_height_m` | >= 1.6 | 2.154 | 2.158 | 2.527 | OK |
| `side_edge_soft_px` | >= 21 | 28 | 46 | 22 | OK |
| `side_blob_frac` | <= 0.25 | 0.1284 | 0.0477 | 0.2187 | OK |

Reported, not barred: `gas_t1_height_m` (0.885-2.521 m, against `t1_footprint_bias_m`), `side_top_m`
(3.723-4.429 m) and `side_bias_m` (1.569-1.902 m).

**`probe.py` exits 1**, on one bar and one seed: at the overview, seed 2's bank now stands only 37.15 deg of hue
from the grass it covers, against the 69.8 g2's captures set. The thinner opacity that gave the side its soft
face also let the grass hue back through at O240 on that patch. The two ways out - raise `GasBankThin` /
`GasBankFull` by 0.10 and reshoot both passes, or re-set the hue bar - are an owner decision, open in
`docs/reference/decisions.md`. Nothing is re-set here: the g2 rule on this build's spread gives
37.15 - 5 = 32.1, a bar the build the critic threw out already passed (its baseline hue was 32.14), and even the
owner-derived floor of 38.0 is still missed by 0.85 deg. A bar is never widened to make a run green.
