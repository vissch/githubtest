# The flamethrower jet's hot core — what was measured (2026-10-19)

The owner's harsh critique scored the special units 31/100 on this effect: "the flamethrower jet is flat
see-through orange cut-outs; the fidelity before/after cannot be told apart", and asked for an opaque white-yellow
core over the first third, orange towards the tip, dark smoke past the tip, and nothing showing through near the
nozzle.

All stills here are the same pose: GreyboxCorridor at night, T2 (zoom 16), yaw 0, `Random.InitState(20250925)`, a
3 s burst from (100, ground, 104) aimed along screen-right, sampled 1.5 s of game time after the burst is created.
`jet-t2-before.png` is that shot on the untouched build, `jet-t2-after.png` on this lane's, and `bg-t2-after.png`
is the identical pose with no fire, which is what `see_through_frac` compares against.

`probe.py` writes `probe.json`. On the committed still:

    core_over_tip_luma = 1.597   (the owner's bar: 1.5)
    see_through_frac   = 0.0000  (the owner's bar: under 0.1)

**Capture noise.** The jet's own animation runs on Perlin and `Mathf.Repeat` sampled at absolute `Time.time`, and
only the delay after the burst is pinned here, not the absolute clock - so two captures of the same build differ.
Measured twice on the untouched build: 181,074 and 201,553 warm pixels, `shotstats` diff mean 6.3. On this build
`core_over_tip_luma` came out 1.526, 1.597, 1.641 and 1.660 over four consecutive captures, so the margin over 1.5
is about one standard deviation wide and the committed still is the one nearest that run's middle. Against the
before still the difference is far outside that noise: `shotstats` diff mean 16.4, 23% of the frame changed.
