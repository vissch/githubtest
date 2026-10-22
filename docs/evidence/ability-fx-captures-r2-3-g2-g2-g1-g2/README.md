# The chlorine bank, round 3-g2-g2-g1-g2: the drawing, and what the erode threshold really does

The owner (2026-10-21): "Draw one soft gas bank a player can see, not a carpet of flat green pads. gas-t1-s0,
gas-t1-s1, gas-t1-s2 and the overhead are tiled rectangles, and `texture_var_drop` passes because those pads are
flat."

The capture half of that was fixed one commit earlier (one bank per band instead of six drifted together). This
round changes **the drawing**, and the stills here are shot on that build: one card per 4 m cell, 10 m wide,
with soft flanks, and a thin cell visibly fainter than a full one.

## The wiring, which two comments and one test had backwards

`DrawGasBank` packs its cards with `Pack(..., frame, 1f, 1f, 0f, kind, GasBankAlpha(thick))`. In `Pack` the
5th argument is **fade** (`m01`, the shader's `i.tone.x`) and the last is **opacity** (`m21`, `i.opacity`). In
`Flipbook_URP.shader` the erode is

```hlsl
half gone = 1.0 - i.tone.x;                        // fade pinned at 1 -> gone = 0
half threshold = gone * (0.45 + 0.55 * edge);      // -> 0 on every gas card
half torn = saturate((tex.a - threshold) / 0.45) * pow(1.0 - gone, 1.5);   // -> saturate(tex.a / 0.45)
```

So **the erode threshold is 0 on every card of this book** and every sheet pixel at alpha 0.45 or more comes out
fully opaque. `GasBankThin` never touched that threshold - it is only the card's own opacity. At 0.95 against
1.0 a thin cell and a full one read the same, which is the "flat pads" half of the complaint, and the old comment
at `CombatFx.Banks.cs:21-24` and the test `BurstRecipeTests.TheGasBankSurvivesTheErodeThreshold` both stated the
wrong wiring. The comment is rewritten in this lane and that test is replaced by
`TheGasBankThinAndFullDoNotReadTheSame`, which checks the thing that is really wired: the opacity spread.

What the build therefore changes:

| Change | Was | Now | Why |
|---|---|---|---|
| `GasBankThin` (the card's opacity) | 0.95 | **0.80** | a thin cell has to read fainter than a full one; the 0.05 spread was invisible |
| the second, wider card per 2x2 block | drawn | **gone** | it was a flat pad stacked on every patch of ground the per-cell cards already covered |
| `_Feather` (new, the gas book only) | - | **0.60** | each card fades out horizontally over 60 % of its half-width, so a carpet of cards is not a carpet of rectangles |
| the per-cell card's width | `6f + 3f * Thick` | **`10f + 3f * Thick`** | a feathered card covers much less than a hard one; the cells now overlap through their soft flanks |

The feather is horizontal only. The card is `Anchored`, its bottom edge is the ground line and the sheet's ink
sits in its bottom third, so a radial feather would eat the drawing itself.

## The readings, before and after

Before = the stills committed at `3b1e25daf3` (the capture fix, drawing untouched). After = the stills here.
Three seeds each, measured by the same `probe.py`.

| Reading | Bar | Before (s0 / s1 / s2) | After (s0 / s1 / s2) | |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0854 / 0.1086 / 0.1220 | **0.0886 / 0.1144 / 0.1215** | OK |
| `texture_var_drop` | >= 0.40 | 0.7300 / 0.7554 / 0.7110 | **0.8308 / 0.8298 / 0.7998** | OK |
| `o240_hue_dist_deg` | >= 108.0 | 135.54 / 138.53 / 126.39 | **133.75 / 137.61 / 122.63** | OK |
| `side_face_px` | >= 30000 | 70300 / **28560** / 116466 | **94847 / 34229 / 130224** | OK (it missed before) |
| `t1_rim_px` | unset | 3 / 2 / 3 | **3 / 2 / 3** | see below |
| `t1_gas_cards` | <= 70 | 90 / 92 / 90 | **68 / 69 / 68** | OK |

`python probe.py` exits **0**: every bar is met by every seed, which the previous capture of this folder could
not say (`side_face_px` missed on seed 1).

## The two bars this round had to derive

The folder's rule: a bar sits **outside** the spread of three captures of the build it guards, and is never
widened to make a run green.

- **`t1_gas_cards` (`<=`)** reads 68 / 69 / 68. `max(seeds)` rounded up to the next 5 = **70**. It is outside the
  spread on the high side, and under 90 - the fewest cards the old carpet build ever drew (90 / 92 / 90) - so the
  bar separates the two builds outright. One bank, 66-68 cells, one card each.
- **`t1_changed_frac`** keeps the owner's 0.08 and is not re-derived. It is the bar the card width was turned
  against, and the captures are the arithmetic: 7 m read 0.0570, 8 m 0.0655, 9 m 0.0773, 10 m **0.0886** on the
  worst seed. Three full two-pass captures, one per width; each is a measurement, none is a bar that moved.
- **`texture_var_drop`** still clears 0.40 (0.7998 at worst), so its bar stays exactly where it was. It was not
  demoted and no owner question was opened: it is simply no longer the only thing watching the T1 frame, since
  `t1_gas_cards` now refuses a carpet by card count.

### `t1_rim_px` stays UNSET, and that is the honest result

Step 7 of the plan wanted the rim bar derived as `floor(min(seeds)) - 1`, above 3, and said to raise `_Feather`
to 0.60 and reshoot if `min(seeds)` came out under 5. That was done - `_Feather` 0.45 -> 0.60, both passes
reshot - and **the reading did not move: 3 / 2 / 3 px at 0.45, 3 / 2 / 3 px at 0.60**, the same as the carpet
build and close to the 1 px the probe's own selftest measures on a hard pad. So the bar would have to be set at
1, at or below the hard pad, where it gates nothing. It is left unset instead of being set to a number that
proves nothing.

Why the median walk does not see the feather, measured on `gas-t1-s0` of the first capture of this build:

- the T1 blob's **median plate difference is 0.0955**, barely above the mask bar `LUMA_BAR` 0.06, so the walk's
  stop level is `0.25 * 0.0955 = 0.0239`;
- the frame's own JPEG noise outside the bank runs 0.00-0.05, so a walk crosses 0.0239 at the first noise dip -
  on the widest row of that still the profile outside the edge reads 0.076, 0.058, 0.046, 0.050, 0.055, **0.006**:
  the walk ends at step 5 on a dip, not where the bank ends.

That the soft flanks ARE in the frame shows in the readings that are not a median of short walks:
`t1_rim_p90_px` rose from 16 / 15 / 20 to **18 / 17 / 23** px, the T1 blob grew from 37402 / 79166 / 105892 to
**59816 / 85237 / 107886** px at the same 68-69 cards, and `texture_var_drop` rose. **The next round needs a rim
reading whose stop level is not inside the frame's noise** (a fixed absolute level, or the walk run on the mean
of several rows), not a bigger feather.

## Reported, not barred

| Row | s0 | s1 | s2 |
|---|---|---|---|
| `t1_rim_p90_px` | 18 | 17 | 23 |
| `t1_blob_px` | 59816 | 85237 | 107886 |
| `t1_luma_delta` | 0.1069 | 0.1377 | 0.1205 |
| `t1_block_var_gas` / `_plate` | 0.000616 / 0.003641 | 0.001121 / 0.006587 | 0.000796 / 0.003978 |
| `gas_t1_height_m` (bias) | 3.278 (3.856) | 5.251 (3.850) | 4.818 (3.719) |
| `gas_side_height_m` | 2.195 | 2.166 | 2.490 |
| `side_top_m` / `side_bias_m` | 3.983 / 1.788 | 3.987 / 1.821 | 4.427 / 1.937 |
| `side_face_frac` / `side_face_col_px` | 0.5913 / 85 | 0.5779 / 81 | 0.5663 / 112 |
| `side_blob_px` / `side_cut_frac` | 160405 / 0.2045 | 59235 / 0.3740 | 229944 / 0.2320 |
| `o240_bank_hue_deg` / `o240_grass_hue_deg` | 88.5 / 222.2 | 86.3 / 223.9 | 98.5 / 221.1 |
| `o240_core_px` | 6926 | 6626 | 5361 |
| `side_tick_skew` | 0 | 0 | 0 |
| `pose_error_m` | 0.0 | 0.0 | 0.0 |

**What `side_face_px` did.** It missed on seed 1 last round (28560 against the 30000 bar) and the README then
called that capture noise on the one seed holding a burning wreck. This round it reads **34229** on that seed,
and 94847 / 130224 on the other two - all three over the bar, and up 35 % / 20 % / 12 % on the drawing change
alone (the capture, the poses and the sim ticks are identical: 217, 1589, 2898, 4239, 5611, 6920, 8261, 9633,
10942 on both passes, `side_tick_skew` 0). So the earlier miss was the thin end of a real spread, and a wider,
softer, overlapping card is what moved it: the bank now has more upright face on screen at every seed. The bar
stays at 30000 and was never touched.

`gas_cells_before` is 0 on all eighteen rows of both manifests, and the worst `draw_calls` is 137 against the
300 ceiling.

## The capture

Both passes, same build, this lane's tip, this checkout's editor closed:

```bash
TW_STILLS_DIR=<dir> "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath trench-warfare-3d -runTests -testPlatform EditMode -testFilter GasBankStills \
  -testResults Logs/g.xml -logFile Logs/g.log        # the gas pass, ~240 s, 9 stills
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...same...        # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2-g2-g1-g2/probe.py --pack <dir>
```

`Unity.exe` is the EDITOR at that path. The `unity.exe` at `%LOCALAPPDATA%/unity/bin/` is the CLI that talks to a
running editor and answers `-batchmode` with "error: unknown option".

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` exits 0 (camera maths, texture drop, hue, face, rim, all hand-calculated); no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |
