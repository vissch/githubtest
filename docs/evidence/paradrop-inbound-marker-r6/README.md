# paradrop-inbound-marker-r6 — the landing puff at the overview, measured

The owner, 2026-10-19: "make sure they all have the proper vfx and that it looks proper ... if things are already
accepted throw a harsh critique agent over them and use the feedback to improve." This folder is the evidence for
the harsh critic's fix 3 on r5 (score 22/100): r5's probe compared a 0-255 channel spread against 0.06, so any
1-level drop counted as dust and `dust_minus_nopuff_px` reached 1442 while the puff was invisible.

## What is here

Six real `CaptureRig` stills from one Play session in `GreyboxCorridor` (night look), all at zoom 240, yaw 21,
pitch 25, lamps (`lights.realLamps` 43, `lights.waterLamps` 0) and the weather clock (`Atmosphere.PinnedClock`
120) pinned, the support disc masked (`CombatFx.ShowOverlays = false`). Every sidecar reads `pose_error_m` 0 and
`mark_on_screen` 1. `DropLandingDustStills.cs` shoots them; `probe.py` measures them and writes `probe.json`.

| still | what it is |
|---|---|
| `z240-before` | landTick - 2, no puff yet |
| `z240-p3`, `z240-p6` | landTick + 3 and + 6, the puff drawn |
| `z240-nopuff-p3`, `z240-nopuff-p6` | the same two frozen frames with the puff's book hidden |
| `z240-p3b` | `z240-p3` re-shot back to back, frozen: the render-noise floor |

## The probe's units are fixed (the measurable half of fix 3)

`probe.py` puts the spread on 0..1 and writes `"spread_scale": 1` to say so. `dust_minus_nopuff_px` is now the
plain count of window pixels (180 x 180 around the mark) that brightened by >= 0.05 luma when the puff's book was
drawn rather than hidden, reported as the **minimum** of the + 3 and + 6 pairs. No morphology, one term.

The noise floor is now honest too. r5 left ten idle frames between `p3` and `p3b`, which sat in a transient bright
state (window mean luma 0.2753 -> 0.305) and made `noise_puff_px` read 5977. Shot back to back, the pair's
sidecar `luma_mean` is identical to five places (0.20603 both) and the measured floor is **`noise_puff_px` 0,
`noise_blob_px` 0**.

## The DustPuff book, measured from the sheet

`Assets/_Project/Resources/VFX/DustPuff.png`, 2048 x 1024, an 8 x 4 sheet of 28 frames. Mean of `alpha * luma`
per cell, frames 0..27:

```
0.0316 0.0498 0.1369 0.1462 0.1476 0.1448 0.1423 0.1424
0.1402 0.1426 0.1377 0.1212 0.1070 0.1070 0.0940 0.0841
0.0730 0.0535 0.0453 0.0369 0.0301 0.0253 0.0191 0.0154
0.0107 0.0079 0.0052 0.0023
```

Maximum 0.1476 at frame 4. The earliest frame at or above 60 % of it was **frame 2** (0.1369, 93 %), which fix 3
part 1 used for `CombatFx.DropDustOpenFrame`. Part 2 (below) re-measures on the sheet's own alpha channel instead
of brightness alone, since the two disagree past the peak, and moves it to frame 3.

## The puff still does not reach the owner's 150 px. Measured numbers

`probe.json`, this run: `dust_minus_nopuff_px` **6** (`_p3` 6, `_p6` 37), `spread_scale` 1, `noise_puff_px` 0,
`landing_dust_px_z240` 27, `luma_delta` 0.1329 against a noise floor of 0.0, `yellow_px_delta` 0.

The shortfall is not the threshold. The whole luma-delta distribution of the two puff pairs, counted down to
0.01 (a fifth of the gate):

| pair | max | px >= 0.01 | >= 0.02 | >= 0.03 | >= 0.05 | >= 0.1 |
|---|---|---|---|---|---|---|
| + 3 | 0.1973 | 20 | 10 | 8 | 6 | 3 |
| + 6 | 0.2973 | 61 | 48 | 43 | 37 | 26 |

Only 20 and 61 pixels of the 32 400-pixel window hold **any** puff ink at all. 150 is therefore out of reach of
brightness at any setting: the ceiling at + 3 is 20 px. It is also out of reach of the planned width lever, and
that lever is a no-op here besides: `DropDustOverview(240) = clamp(240 / 120, 1, 2.2) = 2`, short of the cap, so
raising `DropDustOverviewMax` to 2.6 changes nothing at zoom 240 (checked, then reverted). Count scales with area,
so clearing 150 at + 3 from 6 px would need about 5 x the width — a 50 m puff, far past the 12 m the test caps to
stop the puff swallowing the zone it marks.

Reaching 150 needs the puff's **density** raised: a denser sheet, or several overlapping cards per man. That is a
decision about the art and the recipe, not a knob, and it is left to the owner rather than closed by widening this
probe's window or lowering its 0.05 threshold.

## Fix 3 part 2: the density lever is built, but this harness can never exercise it

`CombatFx.DropDustOpenFrame` is now measured from the sheet's own alpha channel (`probe.py --frames`: fraction of
cell pixels with alpha > 0.1, and mean of `alpha * luma`, printed per frame), not the old brightness-only read:

```
frame  0: coverage=0.0503  1: 0.0682  2: 0.1965  3: 0.2143  4: 0.2305  5: 0.2346  6: 0.2357  7: 0.2412
frame  8: 0.2452  9: 0.2499 10: 0.2463 11: 0.2256 12: 0.2038 13: 0.2039 14: 0.1830 15: 0.1666
frame 16: 0.1469 17: 0.1103 18: 0.0939 19: 0.0793 20: 0.0658 21: 0.0569 22: 0.0443 23: 0.0367
frame 24: 0.0262 25: 0.0197 26: 0.0126 27: 0.0057
```

Frame 9 is the book's maximum coverage (0.2499 of a cell), frame 3 is the earliest at or above 80 % of it
(0.2143, 86 %) — frame 2 (0.1965, 79 %) falls just short. `DropDustCards`
(3), `DropDustCardCount(zoom)` and `DropDustCardOffset(k, width)` stack cards one frame apart on a golden-angle
ring at the overview, cheap (same book, batched) and reversible, as option (a) of the open question below asked
for. `FarGrowTests.ParaDropLandingDustShowsAtOverview` holds all of it, plus the probe's own gate.

**It does not move a single pixel on these stills, and no change inside `CombatFx.OnDropLanded`'s overview branch
ever can.** `CaptureRig.Shot` (`Assets/_Project/Editor/CaptureRig.cs` ~124, ~164) sets the live `TacticalCamera`'s
zoom to the shot's zoom only for the one frame it renders, then restores the camera's own zoom (`keepZoom`)
immediately after. Between shots — including the whole stretch of simulated ticks that crosses `landTick`, where
`OnDropLanded` actually fires — the camera sits at its ordinary zoom. A scratch build with
`Debug.Log("zoom=" + ViewEye.Zoom)` inside `OnDropLanded`, run through this exact test, printed `zoom=30` at every
firing, never 240. `CombatFx.DropDustOverview(30) == 1`, so `overview` is `false` and the small, non-stacked,
frame-0 recipe is what gets baked into the card — regardless of `DropDustCards`, `DropDustWidth`, or
`DropDustOpenFrame`. Raising `DropDustCards` to 8 and rerunning the stills (same build, same test) reproduced the
baseline images **byte-for-byte** (`cmp` against the committed PNGs: identical), which is the proof, not an
inference. `dust_minus_nopuff_px` measured by this probe is therefore reading the puff's close-range recipe
(`2 m` wide, opening on frame 0) viewed through a camera that is only zoomed out for the instant of the picture —
never the overview recipe this whole unit, and r5 before it, was meant to size.

Closing this needs the stills harness itself to hold the camera at the overview zoom through the tick that lands
the drop — not just at each `Shot` call — which is `DropLandingDustStills.cs`, a file this plan marked read-only.
That is a change to the proof, not the recipe, and is the owner's call (or the next leg's, with it put to them):
see the open question below.
