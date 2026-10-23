# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2-g1: the overview cloud, barred, 2026-10-23

**The bar the owner asked for is in and green: `o240_visible_floor` >= 6 on readings of 9, 10 and 10, where the
pre-widening build read 0, 0 and 0.** `python probe.py` still exits 1, on ONE bar, and it is an owner-decided one
I may not move: `green_low_share` >= 0.30 reads **0.2343 / 0.3045 / 0.3067**. The widening lifts the CLOSE bank off
the ground. See "The one failure" and the open row in `docs/reference/decisions.md`.

## The owner's complaint

> At the overview the chlorine bank is invisible. Frames 341, 356 and 371 are one night map, and
> `o240_visible_clouds` reads 0, 0 and 0 with no bar, so the probe exits 0 on the failure. Widen the overview card,
> reshoot three captures at least 60 frames apart, and bar `o240_visible_clouds` so each frame has a cloud a player
> can see. The bar has to sit outside the spread of three captures of the current build.

The widening itself landed earlier on this lane, `95deec7795`: `CombatFx.GasCloudWide = 5.0f`, swept by capture
(visible clouds 0 / 5 / 10 at 3 / 4 / 5), with its `BurstRecipeTests` assert. This folder is that build's evidence.

## "At least 60 frames apart" is now the probe's own refusal

`frame_gap_refusal` replaces `same_frame_refusal`: two runs whose `gas-t1` or `gas-o240` frames are under
`MIN_FRAME_GAP` = 60 apart refuse the probe outright. The same frame is a gap of 0, so nothing the old refusal
caught is lost, and the parent's own 341 / 356 / 371 (gaps of 15) can no longer stand in for fresh captures.
Selftest case (h) checks both by hand: 281 / 341 / 401 pass, 341 / 356 / 371 fail.

## The three captures

`TW_GAS_WAIT_FRAMES` 140 / 200 / 260, each a GAS pass and a PLATE pass into one fresh temp directory at the same
wait, `GasBankStills` by name in a batch editor. All inside the harness's 120..300 clamp and inside the cloud's
life (ticks 60-300). `calls_taken` 5 (O240) and 1 (T1), `pose_error_m` 0 everywhere.

| | run1 (140) | run2 (200) | run3 (260) |
|---|---|---|---|
| frame, `gas-t1` | 744 | 804 | 864 |
| frame, `gas-o240` | 281 | 341 | 401 |
| `o240_visible_clouds` | **9** | **10** | **10** |
| `o240_cloud_count` | 12 | 13 | 13 |
| `o240_cloud_share` | 0.8701 | 0.8913 | 0.9139 |
| `o240_green_area_frac` | 0.0063 | 0.0072 | 0.0073 |
| `o240_green_px` | 6673 | 7453 | 7388 |
| `o240_tank_px` at `o240_med_d` | 312 at 551.5 m | 316 at 547.8 m | 314 at 550.0 m |
| `o240_blob_top` (biggest 3) | 1015, 974, 831 | 1039, 958, 850 | 1048, 944, 880 |
| `green_low_share` | **0.2343** | 0.3045 | 0.3067 |
| `green_height_frac` | 0.1091 | 0.0836 | 0.0829 |
| `green_rise_m` | 3.369 | 2.653 | 2.629 |
| `green_area_frac` | 0.09885 | 0.07577 | 0.07548 |
| `green_soft_share` | 0.1750 | 0.1639 | 0.1661 |
| `t1_top_share` | 0.3758 | 0.2908 | 0.2900 |
| `t1_visible_clouds` | 22 | 24 | 24 |
| `t1_cloud_count` | 38 | 38 | 41 |
| `pose_error_max` | 0.0 | 0.0 | 0.0 |

The overview is fixed, measured: the biggest blob was 119 px before the widening and a tank-square is ~314 px;
now the top blobs are over 1000 px, three tanks' worth, and **9 to 10 of them clear a tank-square in every frame.**

## The bars

| bar | cmp | value | runs | spread | margin | source |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.2343 / 0.3045 / 0.3067 | 0.0724 | **-0.0657** | owner-decided, unmoved, **FAILS** |
| `green_height_frac` | le | 0.15 | 0.1091 / 0.0836 / 0.0829 | 0.0262 | 0.0409 | owner-decided, unmoved |
| `green_area_frac` | ge | 0.05 | 0.09885 / 0.07577 / 0.07548 | 0.02337 | 0.02548 | unmoved |
| `green_rise_m` | ge | 1.8 | 3.369 / 2.653 / 2.629 | 0.740 | 0.829 | re-derived DOWN from 2.4 (the spread is 16x the old one) |
| `green_soft_share` | ge | 0.14 | 0.1750 / 0.1639 / 0.1661 | 0.0111 | 0.0239 | unmoved |
| `t1_top_share` | ge | 0.20 | 0.3758 / 0.2908 / 0.2900 | 0.0858 | 0.0900 | re-derived DOWN from 0.28; the old build's 0.0803 still fails it by 2.5x |
| `t1_cloud_ceiling` | lt | 26 | 22 / 24 / 24 | 2 | 2 | unmoved |
| `o240_green_area_frac` | ge | 0.0001 | 0.0063 / 0.0072 / 0.0073 | 0.0010 | 0.0062 | unmoved |
| `o240_cloud_count` | ge | 3 | 12 / 13 / 13 | 1 | 9 | unmoved |
| `o240_cloud_ceiling` | lt | 15 | 12 / 13 / 13 | 1 | 2 | unmoved - the speck ceiling survived the widening, so it keeps its job |
| `o240_cloud_share` | ge | 0.25 | 0.8701 / 0.8913 / 0.9139 | 0.0438 | 0.6201 | unmoved |
| **`o240_visible_floor`** | **ge** | **6** | **9 / 10 / 10** | **1** | **3** | **NEW, the bar this unit was asked for; the pre-widening build reads 0 / 0 / 0 and fails it outright** |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

**Demoted to a printed diagnostic:** `green_height_floor` (was `green_height_frac` `ge` 0.08). The widening took
the median height DOWN, to 0.1091 / 0.0836 / 0.0829 with a spread of 0.0262, so a floor that cleared its own noise
would have to sit at 0.05 - under `-g1`'s flat-sheet range 0.0741-0.0768, the very reading the floor was built to
fail. Loosening it would re-admit the flat sheet, so it is demoted instead, exactly as the speck ceiling would have
been had it broken. `o240_cloud_count`'s ceiling did NOT need demoting: 12 / 13 / 13 stays under 15 by 2 against a
spread of 1.

## The one failure: the widening lifts the CLOSE bank off the ground

`green_low_share` is the owner's own floor: at least 30% of the green must sit under a 1.8 m head. This build reads
**0.2343** at wait 140 and 0.3045 / 0.3067 at 200 / 260. It is not a capture accident - a wait sweep, two passes
each, says every settled frame sits ON the floor, inside its own noise:

| wait | 140 | 180 | 200 | 240 | 260 | 300 |
|---|---|---|---|---|---|---|
| `green_low_share` | 0.2343 | 0.3028 | 0.3045 | 0.2990 | 0.3067 | 0.3062 |
| `o240_visible_clouds` | 9 | 10 | 10 | 10 | 10 | 10 |

Every window of three waits 60 frames apart inside the 120..300 clamp (120/180/240, 140/200/260, 160/220/280,
180/240/300) therefore fails: either a reading is under 0.30 outright, or the nearest reading clears the floor by
less than the three readings' own spread. 180/240/300 also loses the 60-frame gap at O240 (321 / 387 / 441).
**No capture clears it, and an owner bar is not mine to move**, so this is the open row's question, not a bar I may
loosen. `o240_visible_clouds` is 9-10 at every one of those waits, so the overview is fixed either way.

The paragraph is appended to the existing open question in `docs/reference/decisions.md` ("The close gas: one
cloud, or gas that still pools under a man's head?"), which is the same tension measured from the close side on
2026-10-22. Its options: (a) keep the 0.30 floor and narrow `GasCloudWide` back for the T1 block only (a second
knob and a second sweep, because 5.0 is what buys the overview cloud); or (b) drop the floor to about 0.22, which
this build clears at every wait. The default taken here: nothing moved, the bar left failing and visible.

## The capture recipe

```bash
# from trench-warfare-3d/, this checkout's editor closed, one batch Unity at a time
#   (python Tools/editor_lock.py status first, never the process list)
# per run: the GAS pass, then the PLATE pass, same wait, same fresh temp directory
TW_STILLS_DIR=$TEMP/tw-g1x-run1 TW_GAS_WAIT_FRAMES=140 \
python Tools/pipeline/run_detached.py start g1x-run1-gas --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode -testFilter GasBankStills \
  -testResults $TEMP/tw-g1x-run1/gas.xml -logFile $TEMP/tw-g1x-run1/gas.log
# the same again with TW_GAS_PLATE=1, then:
python docs/evidence/<this folder>/probe.py --pack $TEMP/tw-g1x-run1 --into run1
```

**A trap this leg fell into once:** a temp directory left by yesterday's leg still held ITS plates. The gas pass
only overwrites the gas stills, so the first three packs carried plates from another build (plate frames 301 / 796
against gas frames 281 / 743). Always shoot into a FRESH directory and check that each plate's frame sits near its
gas frame.

## Not touched

`GasPuffBlock`, `GasPuffWide`, `GasPuffStand`, `GasCloudBlock`, `GasCloudWide` (already 5.0 and landed), every
close-gas measure, and the owner's open close-gas row beyond the paragraph this unit's numbers add to it.
`cards/` is the parent's set, carried whole: the close cards are untouched by `GasCloudWide`.

## Checks run

- `python probe.py --selftest` -> exit 0, cases (a) to (h).
- `python probe.py --calibrate` -> the three runs above, frames 744 / 804 / 864 and 281 / 341 / 401.
- `python probe.py` -> **exit 1**, twelve bars OK and `green_low_share` FAILED (above).
- `python ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2/probe.py` -> still exit 0: the parent is untouched.
