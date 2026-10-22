# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g1: the ground mat, and the patch ceiling still red, 2026-10-22

**`python probe.py` exits 1 here.** Part 1 (the measure) and part 2 (the build) are both in: every band of
`PATCH_MIN_PX`+ counts again, the gas bank now draws a GROUND MAT under its upright cards, and `run1`/`run2`/`run3`
are three fresh captures of that build. The mat merged the mass and halved the mud, but it did **not** bring
`bank_patch_count` to the ceiling of 4: these stills read **13 / 17 / 10**. The ceiling stays where the owner's
rule put it and this build fails it. Nothing here was loosened to make it pass.

## The owner's complaint

> At the game camera the three gas-t1 stills are still separate horizontal yellow-green bands with dark mud
> between them. The pass stopped counting thin edges (`bank_thin_patches` 10, 11 and 7) and set the patch ceiling
> at 10, so 4 to 7 thick patches pass. Count the thin bands with the thick ones and put that ceiling outside the
> spread of three captures of the current build.

## The build change: a ground mat

`CombatFx.Banks.cs`, `DrawGasBank`. The geometry says why an upright card cannot close the mud between rows: the
field grid is 4 m, a card's painted gas is about 1.73 m tall, and at the T1 camera a 4 m step in depth projects
about 2.8 m of screen while a 1.73 m upright card projects about 1.2 m. Under half of each row pitch is covered,
and a card tall enough to close it would stand over a man's head (the owner's chest rule).

A card **lying on the ground** has no height to run out of -- its height IS depth. `DrawGasBank` now adds one
`FlipbookFx.Kind.Flat` card per standing cell on a stride of 2 cells in x and z (`GasMatStride`), which cell of the
2x2 block carries it picked by the block's own hash, so no new random draw. The mat card is `GasBankWidth(...) * 2`
(`GasMatWidth`) and square (no `GasBankSquash`: a flat card's height is its depth), at half the upright card's
opacity (`GasMatAlpha` 0.5), on the same frame, mirror and roll hashes. The upright cards are untouched: the mat is
the mass, they are the volume.

Budget, from `run1/manifest.json`: `gas_cards` **967** at t1 (743 before, `MaxCards` 1536), `draw_calls` **133**
(124 before, ceiling 300). 149 cards at O240. Allocation unchanged.

## The runs

Three launches of the new build, each a gas pass and a `TW_GAS_PLATE=1` plate pass, with `TW_GAS_EXTRA_FRAMES`
0 / 30 / 60 so they land on three different sim ticks: t1 `sim_tick` **557 / 597 / 637**, O240 249 / 269 / 289.
`one_frame_fault` passes (no shared tick, no byte-identical still).

## The bars

Re-set from these three captures by the chain's rule -- a bar clears the nearest run by at least the runs' own
spread -- except `bank_patch_count`, which must reject and is left where it was.

| key | runs | spread | bar | margin |
|---|---|---|---|---|
| `bank_low_share` ge | 0.2003 / 0.1870 / 0.1940 | 0.0133 | 0.17 | 1.28x |
| `bank_height_frac` le | 0.1400 / 0.1492 / 0.1438 | 0.0092 | 0.16 | 1.17x |
| `o240_area_frac` ge | 0.0205 / 0.0229 / 0.0232 | 0.0027 | 0.0175 | 1.11x |
| `pose_error_max` lt | 0 / 0 / 0 | 0 | 0.5 | `GasBankStills.PoseBar` |
| `gas_paint_frac` ge | 0.0178 / 0.0179 / 0.0169 | 0.0010 | 0.0155 | 1.4x |
| `gas_green_share` ge | 0.4235 / 0.4030 / 0.4162 | 0.0205 | 0.30 | 5.0x (cards-puff fails by 9.4x) |
| `bank_paint_share` ge | 0.3620 / 0.3120 / 0.3920 | 0.0800 | 0.22 | 1.15x |
| `bank_aspect` le | 1.310 / 2.160 / 2.173 | 0.863 | 3.1 | 1.07x |
| `bank_patch_count` le | **13 / 17 / 10** | 7 | **4** | **-13, FAILED** |

`gas_green_share` rose from 0.21-0.24 to 0.40-0.42 and `bank_paint_share` from 0.19-0.22 to 0.31-0.39: the paint is
greener and much more of it sits in one patch. That is the mat working.

## The patch count is still red

`max + spread` for these runs would be 17 + 7 = **24**, far over the 10 (and never 11) this chain may ship, so
re-setting the ceiling from the new captures is not allowed and the ceiling stays at **4**.

What was tried, each a full capture of its own build:

| mat | `bank_patch_count` | `bank_thin_patches` | `bank_paint_share` | `bank_rise_m` |
|---|---|---|---|---|
| none (the build before) | 15 / 18 / 11 | 10 / 11 / 7 | 0.2037 / 0.1886 / 0.2152 | - |
| stride 2, width x2, alpha 0.5 (**shipped**) | 13 / 17 / 10 | 5 / 8 / 4 | 0.3620 / 0.3120 / 0.3920 | 3.42 |
| stride 2, width x2.4, alpha 0.95 | 21 | 16 | 0.8282 | 5.28 |
| stride 3, width x3.2, alpha 0.95 | 25 | 17 | 0.5701 | 3.97 |

More paint makes ONE mass (`bank_paint_share` 0.83) but more thin fringes at its soft edge, so the count rises, not
falls; and at alpha 0.95 `bank_rise_m` goes to 5.28 m, which puts gas over a man's head and breaks the chest rule.
The shipped mat is the lowest count of the three and the only one that keeps the rise where it was. Getting to 4
separate paint patches needs something this chain has not tried (the thin fringes are card edges and terrain-split
slivers, not rows), so it is left to the next leg rather than bought by loosening the bar.

## The eye check

`run1/gas-t1.jpg` against `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2/run1/gas-t1.jpg` (same pose, same
tick): the horizontal banding is gone and the middle of the field reads as one green wash instead of stripes with
mud between them. Dark terrain still breaks through at the near edge, over the rocks and the shell holes -- that is
the ground showing through a translucent bank, not a row gap, but it is why the owner's "one mass" is not yet a
clean yes.

## The zebra

`python probe.py --against ../ability-fx-captures-r2-2-g2` still exits 1: the zebra reads `bank_patch_count` 40
(ceiling 4), `bank_paint_share` 0.0782 (bar 0.22) and `gas_green_share` 0.2446 (bar 0.30) -- three keys reject it
now, where before only two did.

## `selftest`

Unchanged and green: a 40x40 block plus a 3x100 fringe give `bank_patch_count` 2 and `bank_thin_patches` 1, and the
camera maths and `WIDTH_BRIDGE_PX` span cases pass. `python probe.py --selftest` exits 0.

## No `cards/` here

The isolation stills live in `../ability-fx-captures-r2-2-g2-g1/cards/` and did not change. The in-folder look
report is skipped (`no cards/`); the card rejection check reads that folder directly and passes (`cards-smoke`
0.0908, `cards-puff` 0.1076, both `REJECTED` against the 0.30 bar).

## Out of scope

The gas colour constants (`GAS_DIR`/`GAS_COS`/`GREEN_LEAN`), reshooting `cards/`, the smoke bank, the
predecessor's folder, and landing this lane (the puff brief, `4101179359`, is still unanswered).
