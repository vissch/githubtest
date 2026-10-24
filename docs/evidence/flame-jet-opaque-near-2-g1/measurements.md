# flame-jet-opaque-near-2-g1 - the measured baseline

The owner, 2026-10-24: "jet-t2-1, jet-t2-2 and jet-t2-3 are the same png. A pale straight cut runs through the
underside of the white core, one card meeting the next, and edge_spread 0 is that one frame counted three times.
Draw the near third as one sheet with no cut, shoot three ages of the pinned clock, and put the edge bar outside the
spread of three captures of the current build."

## The harness: three ages, not one age three times

`Tests/Stills/FlameJetStills.cs` pinned ONE age (1.5 s) for all three sessions, so the three captures rendered
byte-identical pngs and `edge_spread` was 0 by construction. It now shoots three ages of the same pinned clock -
1.3, 1.5 and 1.7 s, chosen by `TW_FLAME_CAP` (the plate keeps 1.5) - over a fixed 3 s burst, so every age clears
both valve ramps (0.10 s, 0.14 s) and every `left` (1.7 / 1.5 / 1.3 s) clears the play-out threshold (0.833 s here).
The valve is exactly 1 in all three; only the Perlin and `Mathf.Repeat` phases, which are functions of `PinnedNow`
alone, differ.

Three distinct pngs, measured (md5 of the baseline stills):

    b95996da79f0c619eb63f954f971b2c7  jet-t2-1.png   age 1.3 s
    7e73f1811f22c658283f9b0f55123dcd  jet-t2-2.png   age 1.5 s
    3cc29041d83d8ecdcaf4b88986174d00  jet-t2-3.png   age 1.7 s

`probe.py` now refuses to measure at all if any two of the three share a sha256 or differ by zero in every channel.

## The baseline: the current build, four sessions, 2026-10-24

`baseline/` holds the build at `3738dc6584` (no look change), one editor session per still
(`TW_FLAME_CAP` = 1, 2, 3, plate), same pose and same pinned clock.

| still | age | fire mask x | near px | tip px | near_edge_corr | seam_score | core_over_tip |
|---|---|---|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 624..1249 (626) | 34656 | 4419 | +0.0172 | 1.946 @ -18 deg | 1.961 |
| jet-t2-2.png | 1.5 s | 631..1218 (588) | 33460 | 7018 | +0.0065 | 2.037 @ -16 deg | 2.024 |
| jet-t2-3.png | 1.7 s | 624..1252 (629) | 34801 | 5051 | +0.0153 | 1.900 @ -90 deg | 2.003 |

Triples, and the minimum each bar has to sit outside:

    near_edge_corr   0.0172, 0.0065, 0.0153   min 0.0065
    seam_score       1.9463, 2.0369, 1.8997   min 1.8997
    core_over_tip    1.9610, 2.0237, 2.0029   min 1.9610

## The bars

Each is the midpoint between the baseline minimum and 0, rounded down, so a bar inside the current build's own
spread cannot be passed off as a pass:

| bar | value | why |
|---|---|---|
| `EDGE_BAR` | 0.0032 | half of 0.0065. Four decimals, not two: two would round it to 0.00 and no build could meet it. |
| `SEAM_BAR` | 0.94 | half of 1.8997, rounded down. The cut has to lose half its concentration. |
| `LUMA_BAR` | 1.6 | the owner's standing bar from the round before. The baseline already measures 1.96. |

`edge_spread` is gone: only identical frames could meet it.

`python docs/evidence/flame-jet-opaque-near-2-g1/probe.py --baseline` exits 0 on this evidence: the three frames are
distinct, and both new bars are below their baseline minimum. The full probe (no flag) also wants the new build's
three stills beside it, and the look change that earns them is the next step of this lane.
