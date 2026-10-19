# Paradrop landing dust at zoom 240 — honest units, two ticks (r6)

Harsh critic r1 on r5 scored 22/100 on one finding: **the landing puff is invisible, and r5's probe could not tell.**
`dustprobe3.py` took the channel spread from 0-255 pixel values and compared its delta against `-0.06`, the scale of
the 0..1 luma beside it (lines ~49-53). Any 1-level drop in spread therefore counted as dust, so
`dust_minus_nopuff_px` reached 1442 on render noise while nothing showed to the eye; `yellow_px_delta` passed
trivially (0 before, 0 after).

## What this round changed

- **`probe.py`** (committed here, it measures the stills in this folder). The spread is divided by 255, so both
  terms of the dust mask are on 0..1 — `"spread_scale": 1` in `probe.json` declares it. `dust_minus_nopuff_px` is no
  longer a morphology-fattened mask area: it is the plain count of window pixels that brightened by **>= 0.05 luma**
  when the puff's book was drawn rather than hidden, and it is reported as the **minimum** of the `+3` and `+6`
  pairs, so one number has to hold at both ticks.
  Run against r5's own stills, the fixed probe reads **3 px**, not 1442 — the critic was right.
- **`DropLandingDustStills.cs`** now shoots six stills in one Play session at the same pinned pose
  (`GreyboxCorridor` at night, zoom 240, yaw 21, pitch 25, `lights.realLamps 43`, `lights.waterLamps 0`,
  `Atmosphere.PinnedClock = 120f`, `CombatFx.ShowOverlays = false`): `z240-before` at `landTick - 2`, then
  `z240-p3` / `z240-p3b` / `z240-nopuff-p3` at `landTick + 3` and `z240-p6` / `z240-nopuff-p6` at `landTick + 6`.
  Every sidecar reads `pose_error_m` 0 and `mark_on_screen` 1.
- **`CombatFx.Barrage.cs`** scales the puff at the overview only: `DropDustOverview(zoom)` is
  `clamp(zoom / 120, 1, 2.2)`, so `DropDustWidth(30)` is still exactly 2 m and T3/T2/T1 are untouched by
  construction. At zoom 240 the puff is 10 m (about 32 px at the pose's measured 3.25 px/m), and the card is raised
  to `alpha 0.95`, `glow 2.2` at night, because the scene's `luma_mean` is 0.206 — clearing a 0.05 luma lift needs
  brightness, not width alone.

## The measurement, and where it stands

`python docs/evidence/paradrop-inbound-marker-r6/probe.py` — **exit 1**, short of the 150-px bar:

| key | value |
|---|---|
| `dust_minus_nopuff_px` (min of the two ticks) | 7 |
| `dust_minus_nopuff_px_p3` | 7 |
| `dust_minus_nopuff_px_p6` | **133** |
| `landing_dust_px_z240` | 65 |
| `yellow_px_delta` | 0 |
| `centre_off_px` | 31.5 |
| `noise_puff_px` (the `p3` / `p3b` pair) | 5977 |

Two things this says, both measured, neither guessed:

1. **The puff is real and it grows.** At `landTick + 6` it lifts 133 px by >= 0.05 luma over its own puff-hidden
   twin, against r5's 3. It is close to the bar but not over it. At `landTick + 3` it lifts only 7: the card is three
   ticks (0.05 s) into a 2.33 s life, so it has barely opened. Taking the minimum of the two ticks — the right thing
   to do — is what fails, and the fix is the card's opening, not more width.
2. **The noise pair is not a noise pair.** `z240-p3b` is uniformly brighter than `z240-p3` (window mean luma 0.305
   against 0.2753) and the delta runs one way only (0 px in reverse), while `z240-nopuff-p3`, shot after it, is back
   at 0.2755. So the ten idle frames between `p3` and `p3b` sat in a transient bright state rather than a frozen
   one. The puff pairs are unaffected — each is shot against a twin at the same baseline (0.2753 / 0.2755 at `+3`,
   0.2446 / 0.2445 at `+6`) — but `noise_puff_px` as shot measures that transient, not render noise. The flipbook
   itself runs on scaled time (`FlipbookFx.cs` ~708, ~715: `Time.time` and `Time.deltaTime`), so at `timeScale 0`
   the cards genuinely do not advance; whatever brightened is elsewhere in the frame.

The window stayed at +-90 px throughout. It was not widened to make the number.
