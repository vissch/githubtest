# The chlorine bank, round 3-g1-g2: a side view, and a ceiling on the flat green fill

The owner's words (2026-10-20), on g1's result: "texture_var_drop is 0.70 against a bar of 0.40, which rewards the
flat green fill, and gas_t1_height_m 8.3-9.9 has no side view a player can read as a cloud. Shoot each seed from
the side at zoom 30 and fail a bank whose blocks are flatter than the ground."

Two new bars answer that, and g1's four (`t1_changed_frac`, `texture_var_drop` as a floor, `o240_hue_dist_deg`,
`t1_gap_frac`) stay exactly as they were, as regression bars.

## The side view

`GasBankStills` now takes a THIRD still of every seed: the same cloud, the same zoom (30), the same block - so the
pass timeline, the seed patches and the plate's frame-for-frame match are untouched - but at pitch 8 instead of
25. 8 degrees is `TacticalCamera.PitchMin`, the lowest a player can tilt, and `CaptureRig` clamps a shot to it
anyway, so this is the flattest look at the bank the game ever gives. Stems `gas-side-s0/1/2` and their plates:
18 stills a pass, not 12. Both stills sit inside the existing 520-frame block pad (a block spends about 250), and
there is no second gas call and no second drain.

## The side measure

`side_relief_ratio`, new in `probe.py`. Mask and blob are T1's exactly (`LUMA_BAR` against the plate, then
`largest_blob`). Per blob column with at least `MIN_COLUMN_PX` pixels, the top pixel and the bottom pixel are
turned into heights in metres:

- **the bank's relief** = `p90 - p10` of the per-column TOP height;
- **the ground's relief** = `p90 - p10` of the per-column BASE height. The blob's bottom edge is where the bank
  meets the ground, so it traces the ground's own profile under the bank.
- **the bar** = `side_bank_relief_m / side_ground_relief_m`, higher is better. At or under 1 the bank's blocks are
  flatter than the ground they sit on: the owner's fail.

At 8 degrees the distance of a pixel is *not* taken from the pixel - a near-horizontal ray moves about 0.9 m of
ground distance per pixel row. The distance comes from the SEED (the manifest row's `x`/`z` against the sidecar's
camera) and every column is measured in that one vertical plane: a near-horizontal ray moves only about 0.01 m of
height per metre of distance error, so a fixed plane is well conditioned where the per-pixel one is not. Reported
beside it: `side_bank_relief_m`, `side_ground_relief_m`, `side_top_m`, `side_base_m`, `side_blob_px`, `side_cols`,
`pose_error_m`.

`--selftest` checks the fixed-plane route against the same hand calculation g1 used (the pitch-45 camera's centre
pixel is 0.000 m high at its own 10 m plane, one degree up is 0.34311 m) and `relief([0..10]) = 8.0` (sorted index
9 minus sorted index 1 of 11 values).

## The drop ceiling

`texture_var_drop` keeps its 0.40 floor and gains a ceiling, `DROP_CEILING`, checked `max(got) <= DROP_CEILING`.
A drop is `1 - var_gas/var_plate`, so a ceiling is a floor on how much of the ground texture is still read through
the bank. It is the same number the owner says rewards the flat green fill, barred the other way round, so the
reading cannot be won by painting the frame green.

## Both bars, from this round's own baseline

The control is `f427286f0d` (g1's tip) plus the side still and the probe - no look change: `baseline/` and
`baseline.json`. The family's formulas, and never widened afterwards:

    SIDE_BAR     = baseline_max + max(0.25, spread)      higher is better
    DROP_CEILING = baseline_min - max(0.05, spread)      lower is better (a ceiling)

| Seed | `side_relief_ratio` | `side_bank_relief_m` | `side_ground_relief_m` | `texture_var_drop` |
|---|---|---|---|---|
| s0 | 0.6163 | 4.248 | 6.893 | 0.7210 |
| s1 | 1.0690 | 3.767 | 3.524 | 0.7237 |
| s2 | 0.3252 | 3.679 | 11.312 | 0.6921 |

`side_relief_ratio`: spread = 1.0690 - 0.3252 = 0.7438, margin = max(0.25, 0.7438) = 0.7438,
**`SIDE_BAR` = 1.0690 + 0.7438 = `1.8128`**.

`texture_var_drop`: spread = 0.7237 - 0.6921 = 0.0316, margin = max(0.05, 0.0316) = 0.05,
**`DROP_CEILING` = 0.6921 - 0.05 = `0.6421`**.

Two of the three baseline seeds already read a bank flatter than the ground under it (0.62 and 0.33), and the
third is 1.07 - which is the owner's complaint measured: the bank's top edge rises and falls less than the dirt it
lies on. Every baseline ground relief is 3.5-11.3 m, well over the 0.3 m the plan's risk list set as the point
where the ratio stops being trustworthy, so the metres-instead-of-ratio fallback is NOT used.

The baseline run is the control and exits 1: it misses both bars it sets, and meets all four of g1's.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure: g1's four bars (regression, unchanged) plus `side_relief_ratio` and `DROP_CEILING`. `--selftest`, `--pack`, `--dir`, `--out` unchanged in shape |
| `baseline/` + `baseline.json` | three seeds, three bands, of the build **before** this round's fix (`f427286f0d`) - the control both new bars are set against |

The root of this folder holds no stills of its own yet: the "after" pictures are the next leg's work, and
`probe.py` with no arguments exits 1 until they are captured and packed here.

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass, 9 stills
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, the other 9
python docs/evidence/ability-fx-captures-r2-3-g1-g2/probe.py --pack <dir>
```

## The fix, step by step

| Step | Change | `side_relief_ratio` worst | `texture_var_drop` worst | Note |
|---|---|---|---|---|
| baseline | `f427286f0d`, side still only | 0.3252 (s2) | 0.7237 (s1) | the flat fill the owner described; bars set from here |
| | | | | the next leg fills these rows in |
