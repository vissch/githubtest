# The chlorine bank, round 3-g2-g2-g2-g1: one bank, not a carpet of pads — part 1 (the look change)

The owner (2026-10-21): "Draw one gas bank a player can see, not a carpet of flat green pads. gas-t1-s0 and
gas-side-s0 are tiled pads, and side_face_px is still 135367, 52156 and 186565 against a bar of 41000."

## What made the carpet, read from the shader

`Flipbook_URP.shader:123-126` erodes at `threshold = gone * (0.45 + 0.55 * edge)` with `gone = 1 - tone.x`, and
`tone.x` is the card's **fade** (`FlipbookFx.Pack` puts `fade` in `m01`, `opacity` in `m21`). `DrawGasBank` packs
`fade = 1f`, so `gone = 0`, the threshold is 0 and **nothing is ever eroded at any opacity**; `GasBankAlpha` only
multiplies alpha. r2-3 had raised `GasBankThin`/`GasBankFull` to 0.95/1.0 to "clear the erode threshold" — the
wrong knob — so every standing 4 m cell drew a near-opaque card of near-equal size and height, plus an 11 m body
card per 2x2 block everywhere. That is the carpet.

## The change in this build

- `GasBankThin 0.95 -> 0.30`, `GasBankFull 1.0 -> 0.80`: a translucency knob, the fringe lets mud, wire and men
  through, the core still covers.
- the per-cell card stops tiling: width spread `(0.9 + 0.2*h2) -> (0.7 + 0.6*h2)`, squash scaled by
  `(0.6 + 0.4*Thick)` (a thin cell is a smear, a thick one the full pool), and a per-card ground offset
  `+0.25 * h1`. `GasBankSquash` stays 0.75 (the owner's r1 call: gas pools, it does not stand over a man).
- the body card is drawn only in the cloud's middle (`Count >= 3 && Thick >= 0.55`) and at `alpha * 0.8`:
  one mass with a feathered edge, no blanket.
- `BurstRecipeTests.TheGasBankSurvivesTheErodeThreshold` (which asserted on the wrong knob) is replaced by
  `TheGasBankIsTranslucent_NotAnOpaquePad`.

## The measurement: three of the five bars MISSED on this build

`probe.py` is `ability-fx-captures-r2-3-g2-g2-g2`'s, copied unchanged. `probe.json` is this build's run.

| bar | want | worst of three seeds | |
|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0593 | MISSED |
| `texture_var_drop` | >= 0.40 | 0.6129 | OK |
| `o240_hue_dist_deg` | >= 103.5 | 12.99 (s2) | MISSED |
| `side_face_px` | >= 41000 | 6978 (s1) | MISSED |
| `o240_hue_off_deg` | <= 18.5 | 10.50 | OK |

The three misses are all the same cause: a translucent bank moves fewer pixels far from the plate. `side_face_px`
falling is the plan's own named risk, and the plan's remedy (raise `GasBankThin` by 0.05) only covers
`t1_changed_frac`/`texture_var_drop`, not the hue distance. **No bar was lowered.** Part 2 of the plan (the new
`t1_depth_spread` measure and the `side_face_px` rework the owner asked for) is where this is settled; it was not
done in this leg.
