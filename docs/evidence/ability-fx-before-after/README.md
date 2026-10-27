# ability-fx-before-after

The owner's answer on the Decide page to "ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1-g2: tried 2 times, no
progress": "show me visually before and after". This folder is that picture, built by `beforeafter.py` from
stills already committed to git (no new Unity capture): every gas still from `-g2-g2/` onward shares one camera
(zoom 30, yaw 40, pitch 25, focus 56/142, cam 110.242/33.018/96.486, pose_error_m 0), so the builds are directly
comparable.

## The pictures

- **`gas-t1-before-after.png`** — NO GAS plate / BEFORE (the flat sheet) / AFTER (the GasCloud book), at the T1
  play-zoom camera, each captioned with its measured green lift.
  Sources: `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1/run1/gas-t1-plate.jpg`,
  `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2/run1/gas-t1.jpg`,
  `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1/run1/gas-t1.jpg`.
- **`gas-cards-before-after.png`** — the isolation stills, where the drawing change IS visible: BEFORE / AFTER /
  the smoke card the gas was meant to copy.
  Sources: `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2/cards/cards-gas.jpg`,
  `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1/run1/cards-gas.jpg`,
  `../ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1/run1/cards-smoke.jpg`.
- **`gas-t1-progression.png`** — all six rounds of "tried, no progress" in lane order, each captioned
  "measure only" or "drawing changed": four of the six rounds changed only the probe.

BEFORE is commit `5651e9cb` (the flat-sheet build). AFTER is commit `0fef5e89` (the GasCloud book).

## The measure

"Green lift" = a gas still's green channel minus its own no-gas plate's green channel, averaged inside the union
mask of pixels where that lift is over 8 (0-255 RGB) in either build's run1 `gas-t1.jpg`. Re-measured here by
`beforeafter.py` from the committed jpgs (268,727 px in the union mask):

| | BEFORE (`5651e9cb`) | AFTER (`0fef5e89`) |
|---|---|---|
| mean green lift in mask, runs 1/2/3 | 9.55, 9.84, 9.56 | 12.63, 12.79, 12.44 |
| same-build run-to-run spread | 0.29 | 0.35 |
| cross-build `|dlift|`, all 9 pairs | 2.59 .. 3.25 | |
| whole-frame `|d|` between builds, matched frames | 2.92 | |
| whole-frame `|d|`, same-build run-to-run | 0.12 (before), ~0.1 (after) | |

Bars (`beforeafter.py`, re-measured every run, never hardcoded):
- `layer_cross_min >= 1.5 * layer_noise_max` — PASS (2.59 >= 0.54): the before/after change in masked green lift
  is well over a build's own run-to-run spread, so it is real.
- `after_lift_min >= 11.0` and `before_lift_max < 11.0` — PASS: the two builds' three-run bands (9.55-9.84 vs
  12.44-12.79) do not overlap, with 11.0 in the gap.
- `frame_cross_max < frame_noise_max` — this one is printed as a **FACT, not a pass bar**: it does NOT hold
  (2.92 is bigger than the same-build whole-frame noise). Read plainly: the whole-frame difference between builds
  is small in absolute terms (under 3 of 255), and far smaller than the masked lift change — most of the frame
  (trenches, sky, men) is identical, only the gas patches changed.

## The honest verdict

At zoom 30 the two builds read the same to the eye. Both are a flat carpet of pale green patches lying on the
ground; the plate (`gas-t1-plate.jpg`) has no green at all, so all of the green is gas. The after build is
measurably brighter green (about 30% more lift) inside the same patches, and that is the whole change a player
sees. It is not a standing cloud. This is not sold as more than it is.

## Open

The owner has not yet seen this in Play — these are the same offline stills the probes have used throughout the
lane, not a live capture. The look at T1 is unchanged to the eye; only the green-lift number moved.

The outward brief (step 10 of the plan) was not sent: see the commit/leg note for whether it went out. If it did
not, the three PNGs committed here already satisfy "produced and viewable".
