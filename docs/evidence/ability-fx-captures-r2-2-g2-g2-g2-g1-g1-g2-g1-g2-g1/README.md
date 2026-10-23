# Gas bank captures — the low creep under each puff (unit -g1-g1-g2-g1-g2-g1)

The owner, 2026-10-23:

> At the game camera the three close stills (frames 754, 814, 874) are separate lime bush-puffs with bare ground
> between them. Run1's `green_low_share` is 0.2317 against the owner's 0.30, and this pass dropped the bar to
> 0.11, which clears the spread 0.1156 by 0.006. Put `green_low_share` back to 0.30 and fill the low band of the
> bank so 0.2317, 0.3473 and 0.3042 all clear 0.30. Do not drop the bar.

**The bar is back at 0.30 and this unit's probe is RED on it.** The drawing was changed, not the bar — and
filling the low band turned out to *lower* the measure. The numbers are below; the owner's call is open in
`docs/reference/decisions.md`, "The gas low-band bar 0.30 cannot be reached by filling the low band".

## The production change
`CombatFx.Banks.cs`, the close draw (`step == 1`) only: besides its puff, every standing cell now packs a
**creep** card out of the same `FlipbookFx.Book.GasPuff` (one `DrawPacked`, no new draw call) —
`GasCreepWiden` 3.2 times the puff's width, `GasCreepCardM` 1.7 m tall whatever its width (its ink tops at
1.48 m, under `probe.py`'s `HEAD_M` 1.8), `GasCreepAlpha` 0.7 of the cell's alpha, anchored at `Ground - 0.1`
and boiled half a cycle out of phase with its puff. The puff card, the frame window and the whole overview
branch are untouched. `gas_cards` 106 -> 212, `draw_calls` 134 (manifests in each run).

The look is what was asked for: the 12 m of bare ground between two puffs now has gas lying on it.

## What was shot
`GasBankStills` twice per run into one temp dir (gas pass, then `TW_GAS_PLATE=1` plate pass) at
`TW_GAS_WAIT_FRAMES` 150 / 210 / 270 — the same three waits, so the same frames as the refused set.

| run | wait | o240 frame | t1 frame | calls_taken | pose_error_m |
|---|---|---|---|---|---|
| run1 | 150 | 291 | 754 | 5 | 0 |
| run2 | 210 | 351 | 814 | 5 | 0 |
| run3 | 270 | 411 | 874 | 5 | 0 |

60 frames apart at both bands; worst O240 overlap 0.6641 against `MOVE_OVERLAP_BAR` 0.7021.
`cards/` is carried from `-g1-g2` unchanged: `GasBankCards` hand-packs its own cards and never reads the creep.

## The creep sweep (one gas pass each at wait 210; the plate is gas-free, so one plate serves them all)
| creep | green_low_share | blob_px | green_height_frac | green_rise_m | green_area_frac |
|---|---|---|---|---|---|
| none (`-g1-g2`'s puff-only build) | **0.3473** | 12537 | 0.0933 | 2.529 | 0.0136 |
| widen 2.0, card 1.7 m | 0.2046 | 18749 | 0.1006 | 3.184 | 0.0203 |
| widen 2.0, card 1.2 m | 0.2112 | 17790 | 0.1008 | 3.183 | 0.0193 |
| widen 2.6, card 1.7 m | 0.1994 | 62856 | 0.1440 | 3.934 | 0.0682 |
| widen 3.2, card 1.7 m (kept) | 0.1992 | 75733 | 0.1467 | 4.028 | 0.0822 |

`green_low_share` is read inside `largest_blob` — ONE connected puff. The creep bridges the puffs into one bank,
so the blob grows up to six-fold and takes in far gas whose column-measured height is well over 1.8 m; the low
pixels the creep adds never outweigh them, and the gain saturates once creeps overlap each other (2.6 and 3.2
read the same). Clearing 0.30 by the runs' own spread would need about 0.45 at the old spread; no row comes near.
3.2 is kept because it is the only swept width at which even the thinnest creep (5 * 0.9 * W) reaches the 12 m
spacing of the close cards, which is what fills the bare ground the owner is looking at.

## The bars (`python probe.py`)
Two FAILED, both the owner's own:

| bar | runs | spread | bar | verdict |
|---|---|---|---|---|
| `green_low_share` ge | 0.1979 / 0.1992 / 0.1996 | 0.0017 | **0.30 (the owner's, restored)** | FAILED by 0.1021 |
| `green_height_frac` le | 0.1463 / 0.1467 / 0.1482 | 0.0019 | 0.15 (the owner's ceiling) | FAILED: margin 0.0018 |

The height ceiling fails for the same reason: the blob is now the bank, so its median height is the bank's.
Re-read from these three runs, all OK: `green_rise_m` 3.5 (runs 4.013-4.059), `o240_cloud_share` 0.85 (runs
0.8825-0.9057, the overview draw untouched, these frames' own reading). Carried and OK: `green_height_floor`
0.075, `green_area_frac` 0.0075, `o240_green_area_frac` 0.0065, `o240_cloud_count` 3, `o240_cloud_ceiling` 17,
`o240_blob_floor` 1020, `pose_error_max` 0.50.

### The new measure: `green_top_m`
`measure_t1` now also returns the **p95 of every green pixel's height** (`BAR_TOP_M` 11.0, 'ge'). It is the guard
a flat sheet must fail once the creep pulls the median down: this build reads 12.070 / 12.063 / 12.099, and the
old flat-sheet build's own committed captures read **6.085 / 6.091 / 6.089** with this code —

```bash
python probe.py --calibrate --from docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1
```

`--from DIR` is the probe's third change: read another unit's `run*/` with this code, so an old build's number
can be quoted at all.

## The capture recipe
Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-c32 TW_GAS_WAIT_FRAMES=210 \
python Tools/pipeline/run_detached.py start gas-c32 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/c32.xml -logFile <out>/c32.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1-g2-g1/probe.py --pack %TEMP%/tw-gasbank-c32 --into run2
```

Runs 1 and 3 are the same at waits 150 and 270. Only one batch Unity instance may run against this checkout at a
time. A plate carries no gas cards, so one plate pass per wait serves every build swept at that wait.
