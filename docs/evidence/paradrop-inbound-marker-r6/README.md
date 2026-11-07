# paradrop-inbound-marker-r6 — the landing puff at the overview, measured

The owner, 2026-10-19: "make sure they all have the proper vfx and that it looks proper ... if things are already
accepted throw a harsh critique agent over them and use the feedback to improve." This folder is the evidence for
the harsh critic's fix 3 on r5 (score 22/100): r5's probe compared a 0-255 channel spread against 0.06, so any
1-level drop counted as dust and `dust_minus_nopuff_px` reached 1442 while the puff was invisible.

## What is here

Six real `CaptureRig` stills from one Play session in `GreyboxCorridor` (night look), all at zoom 240, yaw 21,
pitch 25, lamps (`lights.realLamps` 43, `lights.waterLamps` 0) and the weather clock (`Atmosphere.PinnedClock`
120) pinned, the support disc masked (`CombatFx.ShowOverlays = false`). Every sidecar reads `pose_error_m` 0 and
`mark_on_screen` 1. `DropLandingDustStills.cs` shoots them; `probe.py` measures them and writes `probe.json`; the same test writes
`landzoom.json`, the camera's zoom at the instant the drop landed. Both open questions this folder carried
(`docs/reference/decisions.md`) are closed by this run: option (a), the harness holds the overview.

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

## The puff shows: dust_minus_nopuff_px 1913 (gate 150)

`probe.json`, this run: `dust_minus_nopuff_px` **1913** (`_p3` 2681, `_p6` 1913 — the reported number is the
minimum of the two ticks), `spread_scale` 1, `noise_puff_px` 0, `noise_blob_px` 0, `landing_dust_px_z240` 74,
`luma_delta` 0.3746 against a noise floor of 0.0, `centre_off_px` 13.5, `yellow_px_delta` 0.

Nothing in the probe was loosened to get there: the window is still 180 x 180 px around `CaptureRig`'s own screen
projection of the landing point, the threshold is still 0.05 luma, and the count is still one term with no
morphology.

## What actually changed: the harness now stages the scenario

Earlier legs measured 6 px and reported it honestly as a shortfall. The reason was the proof, not the puff.
`CombatFx.OnDropLanded` bakes the puff's width, alpha, glow, open frame and card count from `ViewEye.Zoom` **at
the instant the sim event fires**, and `CaptureRig.Shot` sets the live `TacticalCamera`'s zoom only for the one
frame it renders, restoring the camera's own zoom right after (`Assets/_Project/Editor/CaptureRig.cs` ~124, ~164).
The landing tick falls between shots, so the puff was always spawned with the close recipe (zoom 30: 5 m wide, one
card, frame 0, alpha 0.7, glow 1) and then photographed from zoom 240. Raising `DropDustCards` to 8 reproduced the
stills byte-for-byte, which is how that was proved.

`DropLandingDustStills.cs` now sets `TacticalCamera.Zoom = 240` right after the pins and re-sets it after every
shot drains, so the **whole Play session** runs at the overview — a drop landing while the player is zoomed out,
which is the scenario this unit is about. It is checked, not assumed: a latch on `SimHost.Events` records
`ViewEye.Zoom` at the first `DropLanded` and the test asserts it is at or above `CombatFx.DropDustOverviewFrom`.
`landzoom.json` carries the number: **`zoom_at_droplanded` 240, `droplanded_events` 8**.

The overview recipe is what the new stills hold: width 5 m -> 10 m (4 x the area), the open frame 0 -> 3, alpha
0.7 -> 0.95, night glow 1 -> 2.2, and 3 stacked cards.

**These six stills are a new baseline.** The session no longer snaps to the overview for one frame at a time, so
the men render at their overview tier in every shot, not only in the shot. Comparisons against the older
`z240-*` images in this folder's history are not frame-to-frame comparable; the probe's pairs are, because each
pair is two shots of the same frozen frame.

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

## The open frame and the card stack, measured from the sheet's alpha

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
ring at the overview, cheap (same book, batched) and reversible.
`FarGrowTests.ParaDropLandingDustShowsAtOverview` holds all of it, plus the probe's own 150 px gate.
