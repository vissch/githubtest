# The chlorine bank, round 3: a cloud, not moss

The harsh critic scored round 2-2 at **22/100**. Its words, and where each one is answered:

| The critic said | The answer here |
|---|---|
| "flat olive smears that read as moss (gas-t1.jpg), not a cloud" | `texture_var_drop` and `t1_changed_frac`: a bank must HIDE the ground, not tint it |
| "every bar passed by under 0.025 on one capture" | three seeds a pass, at least 60 m apart; a bar must be met by all three |
| "the committed probe exits 1 on a clean checkout (gas-o240.json missing)" | every still ships with its `.json` sidecar, both pass manifests ship, and `probe.py` reads nothing outside this folder |
| "at O240 it cannot be told from grass" | `o240_hue_dist_deg`: the bank's mean hue against the mean hue of **the same pixels** in the no-gas plate, which is the grass it covers |

The owner's words (2026-10-19) set the bars: a low, soft, semi-opaque gas volume with visible height that hides the
ground under it — at T1 a mean luma change of **>= 0.06** over **>= 8%** of the frame, ground texture variance under
the bank **>= 40% below** the plate, on **3 of 3 seeds** — and a hue **at least 25 degrees** from the plate's grass
at O240.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths and both new readings against hand calculations; no arguments measures the stills beside it and writes `probe.json`; `--dir baseline --out baseline.json` measures the before-pictures |
| `probe.json` | the readings of the build **after** the change, one value per seed, with the bars |
| `baseline/` + `baseline.json` | the same measure on the build **before** the change: the control the bars are set against |
| `gas-t1-s*.jpg`, `gas-o240-s*.jpg` | three seeds, two bands, each with its `-plate` (the same pose and the same sim tick with the gas calls skipped) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still from the harness: the patch, the sim tick, the pose error, the gas field before and after the call, the cards drawn and the draw calls |

The harness is `trench-warfare-3d/Assets/_Project/Tests/Stills/GasBankStills.cs`. Both passes are run on the same
build, frame counts only, never a clock:

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3/probe.py --pack <dir>
```

## The bars

Set against the baseline spread of the **unchanged** build, as `max(owner's bar, baseline_max + margin)`. A bar is
never lowered below the owner's, and never widened to make a run green.

| Reading | Owner's bar | Baseline s0 / s1 / s2 | Margin | **Bar used** |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0436 / 0.0295 / 0.0394 | 0.03 | **>= 0.08** (the owner's: 0.0436 + 0.03 is under it) |
| `texture_var_drop` | >= 0.40 | 0.0531 / 0.0958 / **-0.3144** | 0.10 | **>= 0.40** (the owner's) |
| `o240_hue_dist_deg` | >= 25 | 20.52 / **32.14** / 8.26 | 5 | **>= 38.0** (raised: 32.14 + 5) |

The baseline run of the **unchanged** build misses all three (`baseline.json`, `probe.py --dir baseline` exits 1),
and two of its numbers say plainly what the critic saw:

- `texture_var_drop` is **-0.31** on seed 2: the old bank makes the ground under it MORE textured than the bare
  grass, not less. It is a smear laid over the field, not something lying on top of it.
- `o240_hue_dist_deg` reaches **32.14** on seed 1 all by itself, so the owner's 25 deg would have been passed by
  the very build the critic threw out at 22/100. That is the one bar raised above his, to 38 deg.

Also from the baseline: the bank stands **0.30 - 0.76 m** tall (`gas_t1_height_m`) - ankle-high, where the owner
asked for visible height - and moves only 3-4% of the T1 frame.

`gas_cells_before` in the manifest is the gas field as it stood just before a block's first call, so a leftover
cloud from the previous seed is visible instead of being counted as this seed's gas. Every **O240** row reads 0,
which is what the 1500-frame drain between seeds is for (420 frames, the plan's number, still left 272 cells of
seed 0 in seed 1's overview; that run was thrown away and both passes were reshot). A **T1** row reads a few
hundred because the seed's own overview block gassed the same patch minutes earlier - the same patch by design,
and it is this seed's gas, not another's.

Reported, not barred: `t1_luma_delta` (the mean luma change over the changed pixels), `gas_t1_height_m` (the
"visible height" claim, in metres), `pose_error_m`, and the block variances the drop is computed from.
