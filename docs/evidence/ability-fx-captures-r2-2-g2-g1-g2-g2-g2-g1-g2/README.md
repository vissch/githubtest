# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2: thin edges stop counting, and aspect gets a bar, 2026-10-22

**`python probe.py` clears, exit 0.** Only the MEASURE moves here. `CombatFx.Banks.cs` stays at `3d7aa478ea`
(the g1 build from the predecessor folder); `run1`/`run2`/`run3` are the SAME three stills as
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1/` (t1 `sim_tick` 557 / 597 / 637, twelve distinct hashes), copied
across rather than reshot. The owner's numbers (15/18/11 patches, aspect 5.315/3.014/4.485) ARE those three
captures read by the OLD measure; reusing the pixels is the only way this folder's before/after table shows the
measure change alone, with no tick-to-tick noise mixed in.

## The owner's complaint

> `bank_patch_count` rose from 10, 13 and 7 to 15, 18 and 11, and the ceiling moved from 25 to 26, so the stripe
> key now passes a stripier picture. Stop counting thin card edges as patches, then put the ceiling under 11.
> `bank_aspect` is 5.315, 3.014 and 4.485, spread 2.301, and it is unbarred, so a wide stripe and a short blob
> both pass. After the gaps close, bar aspect inside the spread of three captures.

## Stop counting thin edges

`gas_paint_stats`' patch count used to be a size test alone (`>= PATCH_MIN_PX`, 200 px). The higher-alpha g1 build
pushed `mask_px_raw` from 46,702/50,755/47,337 up to 73,922/77,341/71,307 — half again as much paint — and a lot
of that extra paint was thin fringe a few pixels wide, which cleared 200 px on length alone without ever having a
body.

`patch_core_px` now erodes each patch's own members (`PATCH_ERODE` 4-neighbour passes, a member survives a pass
only if all four of its neighbours are also still in) and a patch counts only when it ALSO keeps a core of at
least `PATCH_CORE_MIN_PX` (60) px. A fringe loses every member within one or two passes and has no core at all; a
card body keeps one. The dropped patches are now printed as `bank_thin_patches` so the gap is visible, not hidden.

`PATCH_ERODE` 2 was tried first and read 10/10/5 (bar would need `max + spread` = 10 + 5 = 15, still over 11).
`PATCH_ERODE` 3 is what the probe ships with: **5/7/4**, spread 3, bar = max + spread = **10**, under 11.

`bank_paint_share` (unbarred-mask-independent: it reads the share of the untouched raw paint mask, not the eroded
one) carries over unchanged at 0.15 — proof the mask itself did not move, only the patch count did.

## Close the gaps in the width

`bank_width_m` used to be `max(gas_cols) - min(gas_cols)`: one stray gas column far from the mass set the width,
which is where the 2.301 run-to-run spread in `bank_aspect` came from. It is now the widest CONTIGUOUS span of
gas columns, bridging holes up to `WIDTH_BRIDGE_PX` (6) columns wide. `bank_aspect` is still
`bank_width_m / med_h`, now of the one mass's own span.

Runs: `bank_width_m` 8.986/8.963/8.405 (down from 13.835/13.415/14.464 — the old formula was counting outlier
columns as "width"). `bank_aspect` 3.452/2.014/2.606, spread 1.438 — down from 2.301 but not closed all the way.

## `bank_aspect`: barred on one side only

The owner asked to bar aspect inside the three captures' spread on both sides (a wide stripe fails high, a short
blob fails low). The rule used throughout this chain is: a bar must clear the nearest run by at least the three
runs' own spread.

- `le` (the wide-stripe side): `max + spread` = 3.452 + 1.438 = 4.890 (bumped to 4.891 to clear a float-rounding
  edge at the exact margin). **This side is barred.**
- `ge` (the short-blob side): would need `min - spread` = 2.014 - 1.438 = **0.576**. That sits at or under 1.0: the
  gap the width fix closed (2.301 -> 1.438) is real but not big enough to set an honest floor — at 0.576 a mass
  barely wider than it is tall would still pass, which is not a floor worth having. `bank_aspect_lo` (the same
  value, printed) stays out of `BARS` and out of `BAR_VALUES`; a future leg that closes the gap further can bar it.

## The one key that lost its zebra margin

`bank_patch_count`'s old job was also to reject the zebra (`--against ../ability-fx-captures-r2-2-g2`): at the
26 ceiling the zebra's 40 failed it by 14. The same opening that gets the runs under 11 also erodes the zebra's
own thin card edges — at `PATCH_ERODE` 3 the zebra reads **6**, inside the new ceiling of 10. Two openings were
tried and neither clears both demands at once:

| `PATCH_ERODE` | runs | bar needed (`max + spread`) | zebra | zebra inside the ceiling? |
|---|---|---|---|---|
| 1 | 13 / 14 / 7 | 21 (over 11) | 37 | no, but the ceiling fails the <11 demand |
| 2 | 10 / 10 / 5 | 15 (over 11) | (not tried further, already over 11) | -- |
| 3 (shipped) | 5 / 7 / 4 | **10** (under 11) | **6** | **yes -- the zebra margin is lost** |

This is the owner's stripe demand ("stop counting thin card edges, ceiling under 11") winning over the zebra
rejection that `bank_patch_count` used to also do. It is not pretended to still hold: `bank_paint_share` (0.0782
for the zebra against a 0.15 `ge` bar, margin 0.0718) is what rejects the zebra now, on its own, unaffected by
the erosion change (it reads the untouched raw paint mask). `python probe.py --against ../ability-fx-captures-r2-2-g2`
still exits 1 for that reason, not because of `bank_patch_count`.

## `selftest`

Two hand-worked cases, in addition to the camera maths: (a) a mask holding a 40x40 block (1600 px; its eroded
core at `PATCH_ERODE` 3 is 34x34 = 1156 px, well over `PATCH_CORE_MIN_PX` 60) and a 3x100 fringe (300 px, over
`PATCH_MIN_PX` but its core erodes to 0 within two passes) must give `bank_patch_count` 1 and `bank_thin_patches`
1 -- called via `patch_core_px` directly, not the whole image path. (b) columns `{10..40}` and `{200..230}` with
`WIDTH_BRIDGE_PX` 6 must give the widest span 31 columns (not 221, the old max-minus-min). Both assert against a
number worked by hand from the construction, not read off the code.

## Before and after

| key | before (old measure, these stills) | after (this measure, same stills) |
|---|---|---|
| `bank_patch_count` | 15 / 18 / 11 | **5 / 7 / 4** |
| `bank_thin_patches` | (not measured) | 10 / 11 / 7 |
| `bank_patch_count` ceiling | 26 | **10** |
| `bank_width_m` | 13.835 / 13.415 / 14.464 | 8.986 / 8.963 / 8.405 |
| `bank_aspect` | 5.315 / 3.014 / 4.485 (unbarred) | 3.452 / 2.014 / 2.606 (barred `le` 4.891) |
| `bank_aspect` spread | 2.301 | 1.438 |

Every other bar (`bank_low_share`, `bank_height_frac`, `o240_area_frac`, `pose_error_max`, `gas_paint_frac`,
`gas_green_share`, `bank_paint_share`) keeps its value from `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1/` and
still clears these same stills -- none of them read the patch count or the width, so they could not move.

## No `cards/` here

Same as the predecessor: the isolation stills live in `../ability-fx-captures-r2-2-g2-g1/cards/` and did not
change. The in-folder look report is skipped (`no cards/`); the card rejection check (`gas_green_share`) reads
that folder directly and is not skippable -- it still passes (`cards-smoke`/`cards-puff` both `REJECTED`).

## Out of scope

The build, the gas hue and its colour constants (`GAS_DIR`/`GAS_COS`/`GREEN_LEAN`), reshooting `cards/`, and
landing this lane (the puff brief, `4101179359`, is still unanswered).
