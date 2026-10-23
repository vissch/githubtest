# The overview clouds are cut, not the bar widened (unit -g1-g2-g2) — part 1

The owner, 2026-10-23:

> At 240 m the gas is flat lime streaks, and the cloud counts are 15, 13 and 13. The owner's ceiling was under 15.
> This pass raised it to 17, so a count of 15 passes with a margin equal to the spread of 2. Put
> `o240_cloud_ceiling` back to 15 and make the overview clouds few enough that all three counts sit under 15 by
> more than their spread. Do not raise the ceiling.

So `o240_cloud_ceiling` is **15** again and the BUILD makes fewer overview cards:
`CombatFx.GasCloudBlock` **12 -> 16** (cells one overview card stands for, 48 m -> 64 m apart).
`GasCloudWiden` 5, `GasPuffBlock` 3 and everything close-camera are untouched, so the T1 bars do not move.

## The sweep (one `GasBankStills` gas pass plus its plate pass each, `TW_GAS_WAIT_FRAMES` 150)

| `GasCloudBlock` | `o240_cloud_count` | `o240_cloud_share` | `o240_blob_max_px` | `o240_green_area_frac` | |
|---|---|---|---|---|---|
| 12 (the 17-ceiling build) | 15 / 13 / 13 | 0.8932 | 1041 | 0.0071 | run1 sat ON his 15 |
| 20 | 6 | 0.6894 | 851 | 0.0023 | count far clear, but no two cards touch, so the biggest blob is one bare card — under the 1000 px the `o240_blob_floor` guard needs to keep refusing the speck build (119 px) |
| **16** | **11** | 0.8555 | **1063** | 0.0061 | **kept**: 11 clears 15 by 4, over the ~2 spread of three 60-frame captures, and the blob floor still holds |

Coverage falls with the cards: `o240_green_area_frac` 0.0071 -> 0.0061 and `o240_cloud_share` 0.8932 -> 0.8555.
Both o240 bars are re-read from this build's three runs (part 2); the owner asked for clouds with gaps, not for
coverage.

`BurstRecipeTests` (19 tests) stays green: a bigger block only widens its "cards stand further apart than the
widest card" margin.

## State of this folder
- `run1/` — this build's wait-150 capture (the block-16 sweep row above), kept as run1.
- `probe.py` — `-g1-g2`'s, with `o240_cloud_ceiling` back at 15. **Every other bar is still `-g1-g2`'s** and is
  re-read in part 2 from run1/run2/run3 of this build.
- Still to come in part 2: `run2/` (wait 210), `run3/` (wait 270), `cards/`, `probe.json`, the bar table.

See `docs/reference/decisions.md`, "The gas bars at a 60-frame capture spread": the ceiling half is answered;
`green_low_share` 0.11 is still the owner's call.
