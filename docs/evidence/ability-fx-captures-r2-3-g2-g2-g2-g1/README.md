# The chlorine bank, round 3-g2-g2-g2-g1: one mass with a feathered rim — part 2 (the drawing)

The owner (2026-10-21): "Draw one gas bank a player can see, not a carpet of flat green pads. gas-t1-s0 and
gas-side-s0 are tiled pads, and side_face_px is still 135367, 52156 and 186565 against a bar of 41000."

## What made the carpet

`Flipbook_URP.shader:123-126` erodes at `threshold = gone * (0.45 + 0.55 * edge)` with `gone = 1 - tone.x`, and
`tone.x` is the card's **fade** (`FlipbookFx.Pack` puts `fade` in `m01`, `opacity` in `m21`). `DrawGasBank` packs
`fade = 1f`, so the threshold is 0 and **nothing is ever eroded at any opacity**; `GasBankAlpha` only multiplies
alpha. So the carpet was not a shader setting: it was the drawing. Every standing 4 m cell got its own card of
near-equal size, plus a body card per 2x2 block, so the cloud's **interior was paved**.

## The change in this build (part 2, steps 6-8)

`DrawGasBank` is now three passes instead of one loop:

1. the standing cells are folded into **16 m blocks** (`4f * MapData.FieldCellSize * step`, was 8 m). No cards.
2. a cell draws a **rim wisp** only where its block earned no mass card: `(4 + 2*Thick) * (0.7 + 0.6*h2)` wide
   (was `6 + 3*Thick`) at `GasBankAlpha(Thick) * 0.7`. Inside a mass, the per-cell cards were pure paving.
3. each mass block draws **one** card at the mean of its cells, `GasMassWidth(Count, step)` wide.

Two pure rules carry it, so a test can hold them (`BurstRecipeTests.TheGasBankIsOneMass_NotAPadPerCell`):

```
GasMassBlock(count, thick) => count >= 4 && thick >= 0.35f     // a crowd that is also thick, not a lucky cell
GasMassWidth(count, step)  => min(28f, 11f + 1.2f * count) * step
```

`GasMassWidth` is 11 m / 28 m, not the plan's 9 m / 22 m: the first reshoot of this build read
`t1_changed_frac` 0.053 against the owner's 0.08, which is the plan's own named risk ("one mass covers less frame
than a carpet did") and its own remedy, applied **before touching any bar**.

The alpha ramp is steeper, not stronger: `GasBankThin 0.30 -> 0.12` (the rim all but vanishes - it is what paved
the frame), `GasBankFull 0.80 -> 0.85` (the mass carries the read). 0.85 still clears
`TheGasBankIsTranslucent_NotAnOpaquePad`'s "even the core is not opaque" (`<= 0.85`), so that test is not weakened.

The card count falls where the mass forms: at O240 (`step = 2`, so 32 m blocks over 2x2-merged cells) `gas_cards`
is **16 / 14 / 22** for 395 / 403 / 314 standing cells, and `draw_calls` never rises (118 / 125 / 112, against
the same three poses' numbers before).

## The measurement: better on every reading, three bars still MISSED

`probe.py` is unchanged from part 1. The 36 stills and `probe.json` here are this build, both passes reshot on
this commit; `pose_error_m` is 0.0 on all 36 and no still was measured with a leftover cloud.

| bar | want | s0 | s1 | s2 | worst | |
|---|---|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0740 | 0.0847 | 0.1328 | 0.0740 | **MISSED** |
| `texture_var_drop` | >= 0.40 | 0.8344 | 0.7015 | 0.7573 | 0.7015 | OK |
| `o240_hue_dist_deg` | >= 103.5 | 132.51 | 135.66 | 126.87 | 126.87 | OK |
| `o240_hue_off_deg` | <= 18.5 | 13.37 | 13.68 | 15.30 | 15.30 | OK |
| `t1_ripple` | <= 0.260 | 0.3349 | 0.3131 | 0.3245 | 0.3349 | **MISSED** |
| `side_ripple` | <= 0.206 | 0.2552 | 0.2719 | 0.2496 | 0.2719 | **MISSED** |

Against part 1's build (the one the owner called a carpet): `o240_hue_dist_deg` went 12.99 -> 126.87 worst and
`o240_hue_off_deg` 10.50 -> 15.30, both now clear; `t1_changed_frac` 0.0593 -> 0.0740; `texture_var_drop`
0.6129 -> 0.7015. `side_face_px`, the retired row, reads 180639 / 28232 / 242539. **No bar was lowered or
widened.**

## Why the two ripple bars did not move, read from the capture

`gas_cards` says it plainly. At T1 (`step = 1`, 16 m blocks) 775 standing cells still drew **684** cards, so
only a handful of blocks earned a mass and the T1 interior is **still carded per cell** - the rim wisps are
narrower and far fainter than the old pads, which is why every other reading improved, but at the 4 m-cell scale
`ripple` measures they are still a step field. The cause is the thickness half of `GasMassBlock`: `Thick` is
`field / 14` (`Refresh`'s `full`), and the T1 blob stands mostly between the 0.8 floor and 4.9 (= 0.35 * 14), so
`blk.Thick >= 0.35f` is false across almost all of it. The gate is wrong for a single-source cloud at the play
zoom, and no width or alpha number fixes it: **the rule has to stop asking for thickness** (a crowd of standing
cells IS the mass; `count` alone, or a thickness read against the block's own maximum rather than the field's
nominal full) before the T1 and side bands can read as one mass.

That is a change to step 7's rule, which this leg's plan fixes exactly as written, so it was not made here.
