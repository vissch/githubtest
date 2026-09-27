# C107: blind check of the tracer-weight knobs (fx.tracerGlow, fx.tracerHaloDepth) against the current default

C107 is meant to win back the barrage weight that C104's order costs. Its knobs are `fx.tracerGlow`, the halo's
colour gain, and `fx.tracerHaloDepth=0`, which stops the halo writing depth so it no longer cuts the smoke, flare
and burst glow behind it. Each candidate was judged as a full shipping set, blind, against today's default (g0).
The C99 pair (`fx.smokeSoft`, `fx.burstGlow`) is not in these sets.

All runs come from one build: release player, scenario barrage, ShelledForest night, T1, tick 140, 64 frames,
`--no-hud`. `hash_start` 252EA3E1DA8F7314 and `hash_end` C8D53F7E42C71459 are equal in all five runs (rule 1).

| run | knobs |
|---|---|
| g0 | none (today's default) |
| gt1 | fx.tracerInSmoke=1, fx.tracerShape=0 (the C104 order with the old wide halo) |
| g1 | gt1 + fx.tracerGlow=1.8 |
| g2 | gt1 + fx.tracerHaloDepth=0 |
| g3 | gt1 + fx.tracerHaloDepth=0 + fx.tracerGlow=1.8 |

## Diff against g0

A pixel counts as changed when any channel moves by more than 8. g0 against m0 is 0.000 on all 64 frames, so the
noise floor is zero.

| frames | gt1 | g1 | g2 | g3 | gt1 vs g2 |
|---|---|---|---|---|---|
| f0-f5 (opening volleys) | 0.016-0.019 | 0.025-0.039 | 0.015-0.017 | 0.025-0.039 | 0.003-0.005 |
| f9-f17 (peak f15) | 0.023-0.040 | 0.036-0.066 | 0.022-0.038 | 0.036-0.066 | 0.002-0.007 |
| f22-f27 | 0.016-0.026 | 0.028-0.038 | 0.015-0.025 | 0.028-0.038 | 0.002-0.003 |
| f30-f41 (lull) | 0.002-0.008 | 0.004-0.011 | 0.002-0.008 | 0.004-0.011 | < 0.001 |
| f44-f57 (late volleys) | 0.012-0.032 | 0.020-0.040 | 0.010-0.030 | 0.020-0.040 | 0.001-0.003 |

The glow gain roughly doubles the changed area. The depth-write change is small: g2 differs from gt1 by at most
0.7%, and g2 is slightly closer to g0 than gt1 is, because the glow behind a round is no longer cut out.

## Blind setup

There were two general-purpose critics, told nothing about knobs, cards or the hypothesis. Each got 12 matched
pairs against g0: 3 frames x 4 candidates. Critic A had f3, f15 and f47, and critic B had f12, f25 and f53, so each
candidate got 6 pairs. Each pair had both full frames plus a 900x600 side-by-side crop. The crop was the same for
every candidate at a frame: the window with the most pixels changed across all four. The pair order and the X/Y
sides were drawn at random (seed 1107), and the critics were told each draw was independent. Each image was scored
0-10 on barrage weight, men readability (naming the trench) and tracers read as bullets in depth. Both critics
judged the left trench.

## Key and scores (w m t; old = g0, new = the candidate)

| critic, pair | run | frame | crop (x0, y0, x1, y1) | new | old | new | d w m t |
|---|---|---|---|---|---|---|---|
| A 6 | gt1 | 3 | 549, 4, 1449, 604 | X | 7 3 3 | 6 3 6 | -1 0 +3 |
| B 1 | gt1 | 12 | 395, 203, 1295, 803 | X | 8 5 3 | 7 5 6 | -1 0 +3 |
| A 1 | gt1 | 15 | 464, 288, 1364, 888 | X | 8 4 3 | 7 4 7 | -1 0 +4 |
| B 3 | gt1 | 25 | 515, 222, 1415, 822 | X | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 5 | gt1 | 47 | 559, 288, 1459, 888 | Y | 7 3 3 | 6 3 7 | -1 0 +4 |
| B 6 | gt1 | 53 | 593, 312, 1493, 912 | Y | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 3 | g1 | 3 | 549, 4, 1449, 604 | X | 7 3 3 | 6 3 6 | -1 0 +3 |
| B 9 | g1 | 12 | 395, 203, 1295, 803 | X | 8 5 3 | 8 5 6 | 0 0 +3 |
| A 9 | g1 | 15 | 464, 288, 1364, 888 | X | 8 4 3 | 8 4 7 | 0 0 +4 |
| B 2 | g1 | 25 | 515, 222, 1415, 822 | Y | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 2 | g1 | 47 | 559, 288, 1459, 888 | Y | 7 3 3 | 6 3 7 | -1 0 +4 |
| B 7 | g1 | 53 | 593, 312, 1493, 912 | Y | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 11 | g2 | 3 | 549, 4, 1449, 604 | X | 7 3 3 | 7 3 7 | 0 0 +4 |
| B 5 | g2 | 12 | 395, 203, 1295, 803 | Y | 8 5 3 | 7 5 6 | -1 0 +3 |
| A 10 | g2 | 15 | 464, 288, 1364, 888 | Y | 8 4 3 | 7 4 7 | -1 0 +4 |
| B 10 | g2 | 25 | 515, 222, 1415, 822 | X | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 12 | g2 | 47 | 559, 288, 1459, 888 | X | 7 3 3 | 6 3 7 | -1 0 +4 |
| B 12 | g2 | 53 | 593, 312, 1493, 912 | Y | 8 4 3 | 6 4 6 | -2 0 +3 |
| A 7 | g3 | 3 | 549, 4, 1449, 604 | X | 7 3 3 | 7 3 7 | 0 0 +4 |
| B 8 | g3 | 12 | 395, 203, 1295, 803 | Y | 8 5 3 | 8 5 6 | 0 0 +3 |
| A 4 | g3 | 15 | 464, 288, 1364, 888 | Y | 8 4 3 | 7 4 7 | -1 0 +4 |
| B 11 | g3 | 25 | 515, 222, 1415, 822 | Y | 8 4 3 | 7 4 6 | -1 0 +3 |
| A 8 | g3 | 47 | 559, 288, 1459, 888 | Y | 7 3 3 | 6 3 7 | -1 0 +4 |
| B 4 | g3 | 53 | 593, 312, 1493, 912 | X | 8 4 3 | 6 4 6 | -2 0 +3 |

### Mean change against g0 (6 pairs each)

| run | weight | men | tracers | weight down in | men down in |
|---|---|---|---|---|---|
| gt1 (order only) | -1.33 | 0.00 | +3.33 | 6 of 6 | 0 |
| g1 (+ glow 1.8) | -1.00 | 0.00 | +3.33 | 4 of 6 | 0 |
| g2 (+ no halo depth) | -1.17 | 0.00 | +3.50 | 5 of 6 | 0 |
| g3 (+ both) | **-0.83** | 0.00 | +3.50 | 4 of 6 | 0 |

### Critic notes (condensed)

- Both critics saw one difference in every pair: whether the tracers are drawn over the smoke or hidden by it. They
  saw no difference in the men, the smoke or the explosions in any pair.
- Drawn over the smoke (g0), the tracers read as flat opaque bars or slabs ("laser sticker"), but the barrage feels
  about 1-2 points heavier because more fire shows.
- Hidden by the smoke, the tracers read as rounds in a 3D smoky space (+3 to +4), but the barrage feels lighter.
  The big dark foreground cloud at f25 and f53 costs the most weight (-2 in every candidate).
- Both critics independently spotted a brighter variant that is hidden by the smoke but has hotter, glowing cores:
  A named p7, p9 and p11, and B named p8, p9 and p11. Unblinded, these are g3, g1 and g2 for A, and g3, g1 and g3
  for B. B called it "the best balance I saw". It wins back weight in some frames, but not enough on average.
- Critic B noted that every variant keeps a constant thick width with a hard edge, which caps depth at about 6.

## Decision: reject every candidate (the weight floor, -0.5 on average)

- The men pass in every candidate: 0 in all 24 pairs.
- The tracers are up in every candidate (+3.33 to +3.50, 6 of 6 each).
- The weight fails in every candidate. The best is g3 at -0.83. It beats gt1 (-1.33) by +0.50, and g1 alone
  (-1.00) gives back +0.33. The glow gain carries most of the recovery. The halo depth write adds little alone
  (-1.17), in line with its diff of under 0.7%.

**Default change pending: reject.** C107's knobs stay at their defaults, which are bit-identical to today:
`fx.tracerGlow=1` and `fx.tracerHaloDepth=1`. The order-only set scored -1.33 here, against -0.33 in c104s-critic.md
(a0044) and -1.00 in d9k-critic.md (a0050). With two more critics against the same default, a0044's pass looks like
the outlier.

What the notes point to: the weight is lost where the dense dark foreground cloud hides whole fans of fire (f25,
f53). A brighter halo does not bring those back. Two directions could: tracers that fade through the cloud rather
than stop at its hard edge, or more glow from the muzzle and impacts in front of the smoke. A stronger glow (above
1.8, with g3) is the cheapest next point to try, though the critics noticed the brighter cores only as subtle.
