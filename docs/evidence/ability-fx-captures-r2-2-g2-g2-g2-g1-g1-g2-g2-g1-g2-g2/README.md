# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2-g2: a cloud at the overview, barred by the tank-square, 2026-10-23

**Done.** Part 1 widened the overview card (`CombatFx.GasCloudWide` 4.0, swept). Part 2 shot this build's three
captures 65 and 70 frames apart, moved both O240 count bars onto the tank-square count, and made the frame rule
the owner's 60-frame gap. `python probe.py` exits **0** with every bar OK.

## The owner's complaint

> The overview pass still counts specks. The biggest green blob is 119 px and the tank-square is about 323 px,
> while `o240_cloud_count` 11/9/7 still clears a ceiling of 15. Make the pass bar the tank-square count, and make
> today's 0, 0 and 0 fail it.

The parent measured the zero: `probe.json` at commit `8e329978c1` reads `o240_visible_clouds` **0, 0 and 0**.
Nothing at the overview was a cloud.

## The fix: a width knob of the overview card's own

`CombatFx.DrawGasBank` drew the merged-out card at `(5 + 2*Thick) * (0.9 + 0.2*h2) * step` - the bare zoom step,
2, so 11.8-16.8 m, about 31-44 px at the overview's 2.6 px/m. `CombatFx.GasCloudWide` replaces the `step` term
there. The close band (`step == 1`, `GasPuffWide`) is untouched, and the cards still stand `GasCloudBlock`
12 x 4 = 48 m apart, so at 4.0 the widest card is 30.8 m and the gaps hold.

## The sweep

One `GasBankStills` gas pass per row at `TW_GAS_WAIT_FRAMES` 240, all three against **one shared plate** (the
plate pass draws no gas cards, so the constant cannot move it), packed and read with `probe.py --measure`:

| `GasCloudWide` | `o240_visible_clouds` | `o240_cloud_count` [`ge` 3, `lt` 15] | `o240_cloud_share` [`ge` 0.25] | `o240_green_area_frac` [`ge` 0.0001] | `o240_blob_top` (1st 3) |
|---|---|---|---|---|---|
| 3.0 | **0** | 14 | 0.6937 | 0.0023 | 302 / 296 / 250 |
| **4.0, KEPT** | **6** | 13 | 0.8578 | 0.0045 | 617 / 580 / 548 |
| 5.0 | 10 | 13 | 0.9125 | 0.0074 | 1046 / 985 / 862 |

`o240_tank_px` reads 324 / 321 / 317 across the rows (the bank's own median distance moves a little as the cards
grow), so the yardstick is the same tank-square the parent measured. 4.0 is the **lowest** row that puts a cloud
of at least a tank-square on the frame at all, and every carried O240 bar still passes there. 3.0 quadruples the
green area and still counts zero clouds: area is not the measure, one blob's size is.

## T1 did not move, but the wait 240 does

`GasCloudWide` only touches the `step > 1` branch, and the sweep proves it: every T1 key reads the same at all
three rows (`green_height_frac` 0.0747, `green_low_share` 0.3363, `green_rise_m` 2.368, `t1_top_share` 0.2577,
`t1_visible_clouds` 24; `t1_cloud_count` jitters 41-42).

**Three carried T1 bars fail at wait 240**, which is the spacing part 2 needs:

| bar | at wait 240 | at the carried waits 200/215/230 | verdict |
|---|---|---|---|
| `green_height_floor` [`ge` 0.08] | 0.0747 | 0.0836/0.0833/0.0841 | **fails** |
| `green_rise_m` [`ge` 2.4] | 2.368 | 2.653/2.643/2.669 | **fails** |
| `t1_top_share` [`ge` 0.28] | 0.2577 | 0.2909/0.2910/0.2901 | **fails** |

The cloud is further through its life at 240 frames: flatter, lower, less merged. This is the plan's own named
risk, and it is now measured, not guessed. Part 2 must pick its three waits (>= 60 apart, inside 120..300) so
that every carried T1 bar holds - 240 alone cannot be one of a 120/180/240 triple unless those bars are re-derived
from the new triple's own spread. No owner-set floor is lowered to make a wait fit.

## The guard test

`BurstRecipeTests.TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat` hard-coded the old step 2
(`(6 + 2) * 1.1 * 2`). It now reads `CombatFx.GasCloudWide`, with its existing assert unchanged: the 48 m gap
must stay wider than the widest card, which is what caps this knob near 6.

## Not touched

`GasPuffBlock`, `GasPuffWide`, `GasPuffStand`, `GasCloudBlock`, every T1 measure and bar, the probe's code, and
the owner's open close-gas row in `docs/reference/decisions.md`.

---

# Part 2: the captures and the bars

## The three captures

Each run is two `GasBankStills` batch passes into one stills directory - the gas pass and the no-gas plate pass at
the same `TW_GAS_WAIT_FRAMES`, so the plate hits the shot's own frame - packed with `probe.py --pack <dir> --into runN`.

| run | `TW_GAS_WAIT_FRAMES` | `gas-t1` frame | `gas-o240` frame | gap to the run before |
|---|---|---|---|---|
| run1 | 165 | 769 | 306 | - |
| run2 | 230 | 834 | 371 | **65** frames, both bands |
| run3 | 300 | 904 | 441 | **70** frames, both bands |

A wait does **not** map one-to-one onto the captured frame: `Block` retries the shot up to three times, 12 frames
each, so a run can sit ~6-12 frames late. Waits 60 apart are therefore not frames 60 apart - the first try,
180/240/300, came out at frames 784/850/904, gaps **66 and 54**, and the 54 is refused. The waits above were
chosen from measured frames, not from the nominal gap.

## Why not 140/200/260 or 180/240/300

Both triples were shot in full and read. They fail on the owner's own floor, not on anything this unit set:

| triple (waits) | frames, T1 | gaps | `green_low_share` [`ge` 0.30, owner-set] | verdict |
|---|---|---|---|---|
| 140 / 200 / 260 | 743 / 803 / 863 | 60, 60 | **0.2331** / 0.3025 / 0.3011 | refused: at 140 the bank is young and tall (`green_rise_m` 3.369, `green_height_frac` 0.1091) and a third of its green is not low |
| 180 / 240 / 300 | 784 / 850 / 904 | 66, **54** | 0.3028 / **0.2990** / 0.3062 | refused twice: the 54-frame gap, and the middle run under the owner's floor |
| **165 / 230 / 300** | 769 / 834 / 904 | **65, 70** | 0.3037 / 0.3034 / 0.3062 | **kept** |

The cloud is only settled from about wait 160 on, and the wait ceiling is 300, so the whole usable band is ~135
frames wide: two gaps of 65 and 70 is as far apart as three captures of this harness can be. No owner-set floor
was lowered to fit a wait.

**The part 1 table above, "T1 did not move, but the wait 240 does", is superseded for wait 240.** Those T1
numbers came from the sweep's **shared plate**, which was shot in a separate pass from the gas rows; a fresh
plate at wait 240 reads `green_height_frac` 0.0835, `green_rise_m` 2.649 and `t1_top_share` 0.2990 - all three
pass. What rules 240 out is the frame gap and the owner's `green_low_share` floor, as the table above shows.

## The bars

Every bar, as `probe.py` prints it on run1/run2/run3. T1 bars are carried from the parent unchanged; the O240 bars
are re-derived from these three readings.

| bar | readings | spread | bar | margin | carried or re-derived |
|---|---|---|---|---|---|
| `green_low_share` | 0.3037 / 0.3034 / 0.3062 | 0.0028 | `ge` 0.30 | 0.0034 | owner-set, carried |
| `green_height_frac` | 0.0835 / 0.0841 / 0.0830 | 0.0011 | `le` 0.15 | 0.0659 | owner-set ceiling, carried |
| `green_height_floor` | same | 0.0011 | `ge` 0.08 | 0.0030 | carried |
| `green_area_frac` | 0.07616 / 0.07606 / 0.07555 | 0.0006 | `ge` 0.05 | 0.0256 | carried |
| `green_rise_m` | 2.650 / 2.669 / 2.634 | 0.035 | `ge` 2.4 | 0.234 | carried |
| `t1_top_share` | 0.2894 / 0.2901 / 0.2903 | 0.0009 | `ge` 0.28 | 0.0094 | carried |
| `t1_cloud_ceiling` | 23 / 23 / 24 | 1 | `lt` 26 | 2 | carried |
| `green_soft_share` | 0.1681 / 0.1637 / 0.1639 | 0.0044 | `ge` 0.14 | 0.0237 | carried |
| **`o240_visible_floor`** | **5 / 5 / 5** | 0 | **`ge` 4** | 1 | **new: the tank-square count** |
| **`o240_visible_ceiling`** | **5 / 5 / 5** | 0 | **`lt` 6** | 1 | **new** |
| `o240_green_area_frac` | 0.0042 / 0.0042 / 0.0047 | 0.0005 | `ge` 0.003 | 0.0012 | re-derived (was 0.0001) |
| `o240_cloud_share` | 0.8293 / 0.8193 / 0.8337 | 0.0144 | `ge` 0.80 | 0.0193 | re-derived (was 0.25) |
| `pose_error_max` | 0 / 0 / 0 | 0 | `lt` 0.50 | 0.50 | `GasBankStills.PoseBar`, carried |
| `o240_cloud_count` | 13 / 12 / 15 | - | **no bar** | - | **dropped**: the speck count the owner refused |

The floor is `min - spread` rounded down and must stay at or over 1. With the readings all 5 the spread is 0, so
the floor sits one cloud under them, at 4, and the margin (1) is over the spread (0). The ceiling cannot be
`lt (max + spread)` = `lt 5` - the readings themselves would fail it - so it is `lt 6`, one cloud over the count.

## The parent build fails the new floor

`docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2/probe.json`, written at commit `8e329978c1`,
reads `o240_visible_clouds` **0, 0 and 0** (`o240_tank_px` 323/324/324). The floor is `ge` 4, so the parent build
fails it outright - the separation the owner asked for. Its `o240_cloud_share` 0.4777/0.4413/0.3676 and
`o240_green_area_frac` 0.0010/0.0008/0.0006 fail the re-derived bars too, and its three captures (frames 804, 819
and 834) are 15 frames apart, so they would now be refused before any bar is read.

## The frame rule

`MIN_FRAME_GAP = 60`, and `frame_gap_refusal` replaces `same_frame_refusal`: for `gas-t1` and `gas-o240` on their
own, any two runs under 60 frames apart are printed and refused. Selftest case (h) hand-counts it: frames from
waits 180/240/300 pass, 180/239/300 fail on 239 - 180 = 59.
