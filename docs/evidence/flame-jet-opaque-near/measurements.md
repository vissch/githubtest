# The jet's near third as an opaque body — what was measured (2026-10-19)

Harsh critic r1 scored the flame jet 41/100 and its fix 1 was: the near third is see-through by eye (hedgehogs and
stakes show through `flame-jet-core/jet-t2-after.png` at x 330-480) while that round's `see_through_frac` of 0.0000
counted any 6/255 of tint as opaque, and its `core_over_tip_luma` of 1.597 lay inside its own noise (1.526-1.660).

## What the build now does

- `Flipbook_URP.shader` has a new `_Solid`. On a solid book the keyed ramp is remapped so anything the artist drew
  goes opaque (`saturate((tex.a - 0.18) / 0.22)`), the depth soften against whatever the card flies over is skipped,
  and the soot thinning (`lerp(0.72, 1, band)`) is turned off. `_Solid` is 0 for all 38 other rows, so every other
  book renders bit for bit.
- `FlipbookFx.Sheets` sets `Solid` on the `JetCore` row alone, and nowhere else.
- `Flamethrower` names the two book rules (`JetLink`, `CoreLink`, `HotThird` 0.34) so they can be asserted, and the
  hot third's stagger card now fires on EVERY hot link instead of every other one, with a third wider pass half a
  step behind it - three overlapping opaque cards per near-third pixel instead of one curl with a hole in it.

Guarded by `FlameJetCoreTests.FlameJetNearThirdHidesGround` and `OnlyTheJetCoreIsSolid`. The first is not vacuous:
with `Solid` taken off the `JetCore` row it fails on that assert (abtest could not be used - it waits on the shared
editor lock, which an interactive editor in another checkout holds).

## The stills

All four are CaptureRig stills with their sidecars: GreyboxCorridor at night, T2 (zoom 16), yaw 0, pitch 25, focus
(100, 104) - critic r1's own pose - a 3 s burst from (100, ground, 104) along screen-right under
`Random.InitState(20250925)`, sampled 1.5 s of game time later with the editor paused and frame-stepped, the
weather pinned (`Atmosphere.PinnedClock` 120) so all four share one sky. `pose_error_m` is 0 on every one and
`blown_frac` 0.0012-0.0070, so nothing is clipping to paper white. `bg-t2.png` is the same pose with no fire.

## The numbers, and the bars this unit did not reach

`probe.py` writes `probe.json` from those pixels. On the committed stills:

    near_edge_corr         = 0.2108   (bar <= 0.15)   per capture 0.157, 0.170, 0.211
    core_over_tip_luma_min = 1.366    (bar >= 1.60)   per capture 1.448, 1.609, 1.366

**Both bars are missed.** They are reported as measured, not argued away.

**Why, and what was tried.** The dominant term is the burst's absolute birth clock, which cannot be pinned from the
editor: the jet's own animation runs on Perlin and `Mathf.Repeat` sampled at absolute `Time.time`, `stepabs` cannot
rewind, and a fresh play session already reads about 80 s before a burst can be fired. Anchoring the birth at
clocks a session can still reach made it worse - a paused editor's stepped frames carry a large, variable
`deltaTime` and the jet's valve ramp runs on it, so the streams came out half developed (20k-56k fire pixels
instead of 130k-190k). Over five capture rounds of the solid build the two numbers moved inside one band:

| round | change under test | near_edge_corr (worst of 3) | core_over_tip_luma_min |
|---|---|---|---|
| 1 | solid core, 3 cards per hot link | 0.164 | 1.520 |
| 2 | mouth glow 2.50 -> 3.20, licks 0.26 -> 0.17, third pass 1.25 -> 1.50 | 0.208 | 1.421 |
| 3 | (round 2 again) | 0.219 | 1.446 |
| 4 | rounds 2-3 reverted | 0.175 | 1.452 |
| 5 | solid edge rule 0.18/0.22 -> 0.10/0.12 | 0.164 | 1.351 |
| committed | rule back at 0.18/0.22 | 0.211 | 1.366 |

Every lever the plan authorised was tried and **measured worse, so each was reverted**: the brighter mouth (the
shader rolls brightness off toward `_Hot` preserving hue, so past about 2.5 the mouth buys almost no luminance
while the extra spill widens the soft outer ring), the wider third pass (a wider card is a bigger soft rim), and
the harder keyed ramp. The spread between captures of ONE build (0.157-0.211, and 1.366-1.609) is itself wider than
the distance left to either bar, which is the same objection the critic raised about r1's number - so the next
round's first job is a capture harness that pins the birth clock, not another look change measured through this
noise. The diagnosis to start from: the near third's mean luminance is 145-154 against a tip of 98-107 with a near
p95 of only 219-225, so the body is not clipping and what holds the mean down is the stream's semi-transparent
outer rim, which is also what carries the plate's edges into `near_edge_corr`.
