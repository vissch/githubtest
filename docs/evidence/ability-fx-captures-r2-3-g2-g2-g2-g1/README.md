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

## Part 2 of the plan, step A: a measure that reads "pads", not "how much gas"

`side_face_px` is retired as a bar. It was cleared **three times over** by the very build the owner called a
carpet (135367 / 52156 / 186565 against 41000) and it spread 12x across three seeds of that one build, so it
could not tell a bank from a carpet - it grows with how much gas there is, and more gas is not what was asked
for. It stays in the rows beside `side_face_frac` and `side_blob_px`, where a number that reports is welcome.

Its replacement is `ripple(delta, blob)`, barred twice: `t1_ripple` and `side_ripple`, both UPPER bounds
(`MAX_BARS`, checked with `<=`), so **fewer pads is the passing side**.

Inside the blob, the per-pixel `|luma_gas - luma_plate|` both bands already build is a proxy for the bank's own
per-pixel alpha, free of the ground's texture. `ripple` averages it over 4x4-pixel tiles into a coarse field `A`
(and the blob's coverage into `W`, keeping only tiles at least half inside the blob), subtracts a 9x9-tile box
mean of `A` - the cloud's own slow dome, which is not a fault - and returns `std(A - smooth) / mean(A)` over the
kept tiles. One mass is smooth at the 4 m-cell scale and reads low; a pad's rim is a step at exactly that scale
and reads high. The ratio is free of how big or how bright the cloud is, which is what `side_face_px` was not.
The box mean is weighted by the kept tiles and wrapped at the edges, so neither the blob's rim nor the frame's
invents a step of its own.

`selftest()` proves it on made-up pixels, both values worked from the definition and not from a run of the code:

| hand case | by hand | read |
|---|---|---|
| a 64x64 delta of 0.40 under a full blob | 0.000 (a flat field has no ripple at any scale) | 0.000 |
| the same with every other 4x4 tile at 0.60 | `(8/81) / 0.50` = 16/81 = 0.198 | 0.198 |

The plan wrote the second as 0.200, assuming `smooth = 0.50` exactly. An odd window over a field of period two
cannot give that: the 9x9 window holds 41 tiles of the centre tile's own parity and 40 of the other, so
`A - smooth = 40 (v - v') / 81 = +-8/81 = +-0.098765`, not `+-0.100`. 16/81 is the same arithmetic carried
through, and it is exact at every tile because the tile grid is periodic and the box mean wraps.

## The baseline, and the two new bars

The current build was reshot on this commit, both passes, three seeds (`baseline.json`; the 18 stills are not
committed, the readings are). It reproduces the committed `probe.json` to within 1e-4 on every bar, which is the
capture's own noise floor - `pose_error_m` is 0.0 on all 18.

| | s0 | s1 | s2 | min | `min * 0.8`, rounded DOWN | bar |
|---|---|---|---|---|---|---|
| `t1_ripple` | 0.3254 | 0.3800 | 0.3768 | 0.3254 | 0.26032 | **0.260** |
| `side_ripple` | 0.2583 | 0.3232 | 0.3003 | 0.2583 | 0.20664 | **0.206** |

For a `<=` bar, "outside the spread" means BELOW the minimum of the three seeds, and the rounding is DOWN so
rounding can only make a bar stricter and never pull it inside the spread (the mirror of `o240_hue_off_deg`'s
round-up). A 20 % margin, because the reading is already a share and so is the gap this build has to close.

No bar was lowered. `t1_changed_frac` (0.08), `texture_var_drop` (0.40), `o240_hue_dist_deg` (103.5) and
`o240_hue_off_deg` (18.5) stand exactly as they were.

## Where this build stands against the six bars

`probe.json` is the committed stills remeasured by the new `probe.py`:

| bar | want | worst of three seeds | |
|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0593 | MISSED |
| `texture_var_drop` | >= 0.40 | 0.6129 | OK |
| `o240_hue_dist_deg` | >= 103.5 | 12.99 (s2) | MISSED |
| `o240_hue_off_deg` | <= 18.5 | 10.50 | OK |
| `t1_ripple` | <= 0.260 | 0.3800 (s1) | MISSED |
| `side_ripple` | <= 0.206 | 0.3232 (s1) | MISSED |

Four misses, and they are now the right four: the drawing still puts one near-equal card on every standing 4 m
cell. Step B of the plan (the cell loop in `CombatFx.Banks.cs`) is what has to clear them; it was not done in
this leg.
