# ability-fx-captures-r2-2-g2-g1-g2: three captures on three frames, and a bar on the paint, 2026-10-21

**`python probe.py` clears, exit 0.** The build it bars has not moved: it is still the gas bank rewrite in
`Presentation/Camera/CombatFx.Banks.cs` (commit `5c0c33620a`) — wide overlapping cards at low alpha, with the cell
hash breaking the top line, the brightness and the roll. What changed here is only the capture and the measure.

## The owner's complaint

> The pass sets `bank_patch_share` at 0.20 between the zebra's 0.1863 and this build's 0.2070, and the three runs
> spread 0.0001. `run2/gas-o240.jpg` and `run3/gas-o240.jpg` are one file. `gas_hue_window_px` is 1, so the barred
> mask is 4324 of about 46700 plate-difference pixels, and `draw_calls` stayed 133, the zebra's count. Count the
> paint in the frame and put the bar outside the spread of three captures that are not the same frame.

Both faults confirmed before anything was changed: all three g1 runs were t1 `sim_tick` 557 / o240 `sim_tick` 249
— **one frame, three times** — and `run2/gas-o240.jpg` and `run3/gas-o240.jpg` were md5-identical. So g1's 0.0001
spread was JPEG jitter, and every bar set one spread outside it was safe by construction. And `bank_patch_share`
divided the barred patch (4,324 px) by the raw plate difference (46,702 px): nine tenths of that denominator is
the six moving infantry and JPEG block noise, not gas.

## What is here

- `run1/`, `run2/`, `run3/` — three runs of the four `GasBankStills` captures plus
  `manifest.json`/`manifest-plate.json`, each from its own pair of batch launches (gas pass, then the same with
  `TW_GAS_PLATE=1`). **The runs stand on three different frames**: `gasOffsetFrames` 0 / 30 / 60 (the new
  `TW_GAS_OFFSET_FRAMES` knob, part 1 of this lane) puts t1 on `sim_tick` 557 / 597 / 637 and o240 on 249 / 269 /
  289. Every manifest row has `books_ready: true`, `calls_taken` 1 (t1) / 5 (o240) and `pose_error_m` 0. All
  twelve jpgs have twelve different hashes.
- `probe.py` — g1's probe with the paint mask, the three paint keys and the one-frame refusal. The camera maths,
  `blobs`, `--selftest`, `--pack`, `--against` and the metre keys' mask are untouched, so `bank_low_share` and
  `bank_height_frac` stay comparable with g1 and g2.
- `probe.json` — all three runs' numbers, their `sim_tick`s and `BAR_VALUES`.
- No `cards/` here: the isolation stills did not change, they are in
  `../ability-fx-captures-r2-2-g2-g1/cards/`, and the look report is skipped (`no cards/`).

## The mask

Two masks now, on the same stills.

- **The metre mask, unchanged from g2/g1**: a pixel must differ from the plate by >= 20 RGB (T1) / >= 12 RGB
  (O240), sit inside the single largest coherent plate-difference patch (T1 only), and be brighter than the plate
  (Rec.709 luma >= 2). `bank_low_share`, `bank_height_frac` and `bank_rise_m` still come off exactly this.
- **The paint mask, new** (`paint_stats`): plate difference **AND** brighter than the plate, over the **whole
  frame**, with no patch restriction at all. The gas is a lit card over a dark night field, so it lifts luma;
  infantry motion and JPEG noise change luma both ways. This is what "count the paint in the frame" asks for, and
  it is the denominator of the new share — 24,631-28,355 px of paint, not 46,702 px of any difference.

`mask_px_raw`, `mask_px_patch` and `gas_hue_window_px` are still printed per run, unbarred, so the cut each mask
makes stays visible. `bank_patch_share` (the ratio the owner quoted) is **dropped**.

## The paint keys

| key | cmp | what it is |
|---|---|---|
| `paint_px`, `paint_frac` | `ge` | paint pixels, and paint / 1280x720: how much of the frame the gas paints |
| `bank_paint_share` | `ge` | paint in the biggest paint patch / `paint_px`: one bank is one body |
| `bank_patch_count` | `le` | paint patches of 200 px or more (`PATCH_MIN_PX`): a zebra is many cards |

**The zebra against this build**, both measured by this probe
(`python probe.py --against docs/evidence/ability-fx-captures-r2-2-g2`, which always exits 1):

| build | `paint_frac` | `bank_paint_share` | `bank_patch_count` |
|---|---|---|---|
| the zebra (g2, `843a03570c`, t1 tick 557) | 0.0429 | 0.0782 | 40 |
| this build, run1 / run2 / run3 | 0.0267 / 0.0308 / 0.0272 | 0.1855 / 0.1622 / 0.1936 | 10 / 13 / 7 |

On the paint the gap is wide, far wider than it was on the raw difference: the share doubles and the patch count
drops from 40 to 7-13. `paint_frac` goes the other way — the zebra painted more of the frame (0.0429) than this
build does (0.027-0.031), which is the same thing as "it is fainter than the zebra was" below. `paint_frac` is
therefore barred only as a floor under this build, not as a zebra test.

## The bars

Set from two measured sources only: the three runs above, and the zebra re-measured by this probe. A bar must
clear the nearest run by at least the three runs' spread; a zebra bar must also leave the zebra failing by more
than that spread.

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.07 | 0.4371 / 0.2584 / 0.2853 | 0.1787 | 0.1884 | the roof the real spread allows (was 0.30) |
| `bank_height_frac` | `le` | 0.27 | 0.0799 / 0.1711 / 0.1608 | 0.0912 | 0.0989 | the floor the real spread allows (was 0.15) |
| `o240_area_frac` | `ge` | 0.010 | 0.0130 / 0.0150 / 0.0153 | 0.0023 | 0.0030 | g2's bar, kept — it still clears |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `paint_frac` | `ge` | 0.0220 | 0.0267 / 0.0308 / 0.0272 | 0.0041 | 0.0047 | below the lowest run by more than the spread |
| `bank_paint_share` | `ge` | 0.12 | 0.1855 / 0.1622 / 0.1936 | 0.0314 | 0.0422 | zebra 0.0782 fails it by 0.0418 |
| `bank_patch_count` | `le` | 25 | 10 / 13 / 7 | 6 | 12 | zebra 40 fails it by 15 |

**The honest cost of three real frames.** The spread is no longer ~0, because the bank *rises* as it ages: over
80 ticks `bank_rise_m` goes 1.845 -> 3.443 -> 3.094 m, so `bank_low_share` falls 0.4371 -> 0.2584 and
`bank_height_frac` climbs 0.0799 -> 0.1711. Both of those bars had to move outside that real spread, so they are
weak now (0.07 and 0.27 instead of the owner's 0.30 and r2's 0.15); they guard against the gas vanishing or
filling the screen, nothing finer. The offsets were **not** shrunk to make the spread small — that is the fault
being fixed. The two zebra bars and `paint_frac` still hold with room to spare.

## Refusing one frame

`one_frame_fault` runs before any bar is read and exits 1 with `three captures, one frame` if two runs share a t1
`sim_tick`, or if any two of the twelve jpgs are byte-identical (sha1). g1's evidence would have been refused on
both counts. Every run's `sim_tick`s are printed and written into `probe.json`.

## For the owner: the look

`run1/gas-t1.jpg`, `run2/gas-t1.jpg` and `run3/gas-t1.jpg` are now the bank at three ages (ticks 557, 597, 637)
instead of one frame copied three times — the body drifts and lifts between them, which is the point of shooting
three frames. The stripes are gone; the gas reads as one body. It is **still fainter than the zebra was**: it
paints 0.027 of the frame against the zebra's 0.043. `draw_calls` on t1 is 133 / 124 / 120 across the three ages
(the zebra's count at the same age, 133), so the wider cards still cost nothing. Making the bank both one mass and
strongly visible needs a denser gas sheet, not another alpha number — out of this lane's scope, as in g1.
