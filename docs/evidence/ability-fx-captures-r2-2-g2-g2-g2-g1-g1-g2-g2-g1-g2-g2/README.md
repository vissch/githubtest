# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2-g2: a cloud at the overview, swept, 2026-10-23

**Part 1 of 2, done: the look moved.** This part widens the overview card and sweeps it by capture. It sets
**no bar** and carries **no run captures**: `probe.py` here is the parent's file byte-for-byte, and `cards/` is
the parent's one `GasBankCards` launch. `python probe.py` therefore exits 1 ("three runs needed: found 0") until
part 2 shoots `run1/`, `run2/` and `run3/` and sets the floor on `o240_visible_clouds`.

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
