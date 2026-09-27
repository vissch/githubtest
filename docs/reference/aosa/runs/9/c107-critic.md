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

## Round 2: stronger glow (g4 = g3 with fx.tracerGlow=2.6, g5 = g3 with fx.tracerGlow=3.5)

| run | knobs |
|---|---|
| g4 | fx.tracerInSmoke=1, fx.tracerShape=0, fx.tracerHaloDepth=0, fx.tracerGlow=2.6 |
| g5 | fx.tracerInSmoke=1, fx.tracerShape=0, fx.tracerHaloDepth=0, fx.tracerGlow=3.5 |

These come from the same build and the same battle: `hash_start` and `hash_end` equal g0's.

### Diff

- **The footprint does not grow.** The changed fraction against g0 is the same as g3's on every frame (for
  example 0.066 at f15, 0.038 at f25, 0.040 at f53). The extra glow only brightens pixels that g3 already
  changed.
- **The change is in brightness.** Against g3, g4 differs by 0.013-0.044 in the volleys, and g5 by 0.013-0.046.
  g4 and g5 differ from each other by 0.016-0.039 (f3-f53), with a max channel step of about 60.
- **Clipping grows.** Pixels with any channel at 250 or more at f13: g0 4.4%, g3 4.8%, g4 6.8%, g5 7.2%. At f25: g0
  1.7%, g3 1.9%, g4 2.6%, g5 2.8%. Most of the rise from g3 to g4 is new clipping, and g5 adds little over g4.

### Blind setup

There were two general-purpose critics, C and D, with the same scoring prompt as round 1. Each was also asked for
a tracer-look tag per image: bullets, lasers, blown-out, dim or mixed. Each got the same 15 pairs against g0:
g4 and g5 at f3, f12, f15, f25, f47 and f53, plus g3 as an anchor at f15, f25 and f53. The anchor compares these
critics with A and B. C drew order and sides with seed 1108, and D with seed 1109. The crops are round 1's crops
for each frame. Both critics judged the left trench.

### Key and scores (w m t; old = g0, new = the candidate)

| critic, pair | run | frame | new | old | new | d w m t | new tag (C / D) |
|---|---|---|---|---|---|---|---|
| C 14 | g3 | 15 | X | 9 4 3 | 9 4 6 | 0 0 +3 | mixed |
| C 12 | g3 | 25 | Y | 7 4 3 | 7 4 7 | 0 0 +4 | bullets |
| C 1 | g3 | 53 | X | 7 5 3 | 7 5 7 | 0 0 +4 | bullets |
| C 6 | g4 | 3 | Y | 6 4 4 | 6 4 7 | 0 0 +3 | bullets |
| C 8 | g4 | 12 | Y | 8 4 3 | 8 4 6 | 0 0 +3 | mixed (near-white cyan) |
| C 10 | g4 | 15 | Y | 9 4 3 | 9 4 6 | 0 0 +3 | mixed (near blow-out) |
| C 5 | g4 | 25 | Y | 7 4 3 | 7 4 7 | 0 0 +4 | bullets |
| C 11 | g4 | 47 | X | 7 5 3 | 7 5 7 | 0 0 +4 | bullets |
| C 13 | g4 | 53 | X | 7 5 3 | 7 5 7 | 0 0 +4 | bullets |
| C 4 | g5 | 3 | X | 6 4 4 | 6 4 7 | 0 0 +3 | bullets |
| C 15 | g5 | 12 | X | 8 4 3 | 8 4 6 | 0 0 +3 | mixed (near-white cyan) |
| C 3 | g5 | 15 | X | 8 4 3 | 8 4 6 | 0 0 +3 | mixed (near-white cyan) |
| C 9 | g5 | 25 | Y | 7 4 3 | 7 4 7 | 0 0 +4 | bullets |
| C 2 | g5 | 47 | X | 7 5 3 | 7 5 7 | 0 0 +4 | bullets |
| C 7 | g5 | 53 | Y | 7 5 3 | 7 5 7 | 0 0 +4 | bullets |
| D 5 | g3 | 15 | X | 8 4 3 | 7 4 6 | -1 0 +3 | bullets |
| D 11 | g3 | 25 | X | 7 4 3 | 6 4 6 | -1 0 +3 | bullets |
| D 2 | g3 | 53 | X | 7 4 3 | 5 4 6 | -2 0 +3 | mixed |
| D 8 | g4 | 3 | Y | 6 5 3 | 6 5 6 | 0 0 +3 | bullets |
| D 1 | g4 | 12 | X | 7 4 3 | 6 4 6 | -1 0 +3 | bullets |
| D 7 | g4 | 15 | X | 8 4 3 | 7 4 6 | -1 0 +3 | bullets |
| D 15 | g4 | 25 | Y | 7 4 3 | 6 4 5 | -1 0 +2 | mixed |
| D 9 | g4 | 47 | Y | 7 4 3 | 6 4 6 | -1 0 +3 | bullets |
| D 12 | g4 | 53 | X | 7 4 3 | 5 4 6 | -2 0 +3 | mixed |
| D 14 | g5 | 3 | X | 6 5 3 | 6 5 6 | 0 0 +3 | bullets |
| D 3 | g5 | 12 | Y | 7 4 3 | 6 4 4 | -1 0 +1 | blown-out |
| D 6 | g5 | 15 | Y | 8 4 3 | 7 4 4 | -1 0 +1 | blown-out |
| D 13 | g5 | 25 | X | 7 4 3 | 6 4 5 | -1 0 +2 | mixed |
| D 10 | g5 | 47 | X | 7 4 3 | 6 4 5 | -1 0 +2 | mixed |
| D 4 | g5 | 53 | Y | 7 4 3 | 5 4 5 | -2 0 +2 | blown-out |

The tag column gives C's tag for C's rows and D's tag for D's rows. g0 was tagged "lasers" in every pair by both
critics.

### Mean change against g0

| run | critic C (6) | critic D (6) | pooled C+D (12) | weight down in |
|---|---|---|---|---|
| g4 (glow 2.6) | 0 / 0 / +3.50 | -1.00 / 0 / +2.83 | **-0.50 / 0 / +3.17** | 5 of 12 |
| g5 (glow 3.5) | 0 / 0 / +3.50 | -1.00 / 0 / +1.83 | **-0.50 / 0 / +2.67** | 5 of 12 |
| g3 anchor (glow 1.8) | 0 / 0 / +3.67 (3) | -1.33 / 0 / +3.00 (3) | -0.67 / 0 / +3.33 (6) | 3 of 6 |

Each cell reads weight / men / tracers.

- **Critic C scored weight 0 in all 15 pairs, the g3 anchors included.** Critics A, B and D all scored g3 lower
  (A and B -0.83, D -1.33), so C does not tell the looks apart on weight. C's zeros are what lift the pooled
  weight to exactly -0.50.
- **Critic D saw the weight difference.** For D, g4 recovers +0.33 over the g3 anchor (-1.00 against -1.33), and
  g5 recovers the same +0.33. Carrying that step over to A and B's g3 score (-0.83) puts g4 at about -0.50.
- **f25 and f53 are unchanged.** D scored g4 and g5 at -1 and -2 there, the same as g3. A brighter halo does not
  bring back the fire the dark foreground cloud hides.

### Lasers and blow-out

- **g4 still reads as bullets.** D tagged it bullets in 4 of 6 pairs and mixed in 2. C tagged it mixed at f12 and
  f15, where the dense upper-left bundle goes near-white cyan and close to blowing out.
- **g5 starts to blow out.** D tagged it blown-out in 3 of 6 pairs (f12, f15, f53). D wrote that the cyan-mint
  cores are near clipped white and "read as neon beams / lasers". D's g5 tracers score fell to +1.83, against
  +2.83 for g4. C saw the same near-white cyan at f12 and f15.
- **Both critics raised the colour unprompted.** The brighter glow shifts the green toward cyan. C called it "a
  little sci-fi" and suggested a warmer or yellower core. D said the pure green with a soft glow reads most like
  real rounds.

## Round 2 decision: reject g5, and reject g4 as not shown to pass

- **g5 fails.** Its weight equals g4's, its tracers are lower (+2.67 pooled, +1.83 for D), and D tagged it
  blown-out or laser-like in 3 of 6 pairs.
- **g4 sits exactly on the floor.** Pooled, its weight is -0.50. The literal floor is "not below -0.5", so on the
  letter g4 passes: men 0 in 12 of 12 pairs, tracers +3.17. But the only discriminating critic scored it -1.00,
  and the other critic gave 0 even to the g3 anchor that three critics scored lower. The pass rests on a critic
  that did not measure weight.
- **The best estimate is borderline.** Anchoring g4 to the round 1 critics gives about -0.50: a borderline case,
  not a pass.

**Default change pending: reject.** If the coordinator accepts the pooled pass as the letter of the rule, the set
would be `fx.tracerInSmoke=1,fx.tracerShape=0,fx.tracerHaloDepth=0,fx.tracerGlow=2.6`. Glow does not scale past
2.6: g5 adds no weight and starts to clip. Two directions are left, both untried:

- the tracers fade through the cloud instead of stopping at its edge (f25 and f53);
- a warmer, less cyan core at glow 2.6.
