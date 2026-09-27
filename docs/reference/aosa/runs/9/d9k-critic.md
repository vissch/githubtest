# C99 + C104 defaults together (d9k, d9kt): blind check of the proposed default set

The proposed default set is `fx.smokeSoft=0.6,fx.burstGlow=0.5,fx.tracerInSmoke=1,fx.tracerShape=0`. Each part
passed a blind critic on its own: C99 in batch9-critic.md (weight +0.17, men +0.17) and C104 at shape 0 in
c104s-critic.md (tracers +2.2, weight -0.33, men 0). The two parts had never been judged together, so this is that
check.

All four runs come from one build: release player, scenario barrage, ShelledForest night, T1, `--no-hud`.

| run | knobs | tick | frames |
|---|---|---|---|
| m0 | none (today's defaults) | 140 | 64 |
| d9k | the proposed set | 140 | 64 |
| b0t | none (today's defaults) | 180 | 32 |
| d9kt | the proposed set | 180 | 32 |

## Diff

A pixel counts as changed when any channel moves by more than 8. For the noise floor, s0 and b0 were each compared
with m0: both give 0.000 on all 64 frames, so the build is deterministic.

- **d9k vs m0**: every frame differs. The changed fraction is 0.020-0.032 at f0-f11, peaks at 0.044-0.062 at
  f12-f27, falls to 0.011-0.022 in the f28-f43 lull, peaks again at 0.042-0.060 at f50-f58, and ends at 0.020 by
  f63.
- **d9kt vs b0t**: every frame differs. The changed fraction runs from 0.024 at f0 to 0.046 at f24-f25.

## Blind setup

There was one general-purpose critic, told nothing about knobs, cards or what changed. It judged 8 matched pairs:
5 from the tick-140 window (f3, f15, f25, f40, f53) and 3 from the tick-180 window (f9, f24, f30). Each pair had
both full frames plus a 900x600 side-by-side crop of the window with the most changed pixels. X and Y were drawn at
random per pair (seed 1099), and the critic was told the draw was independent. Each image was scored 0-10 on
barrage weight, men readability (naming the trench), tracers read as bullets in depth, and smoke.

## Key and scores (w m t s; old = defaults, new = the proposed set)

| pair | run | frame | crop (x0, y0, x1, y1) | new | trench | old w m t s | new w m t s | d w m t s |
|---|---|---|---|---|---|---|---|---|
| 1 | d9k | 3 | 790, 160, 1690, 760 | X | left | 7 4 5 6 | 6 4 6 6 | -1 0 +1 0 |
| 2 | d9k | 15 | 560, 290, 1460, 890 | X | left | 8 4 4 6 | 7 4 7 7 | -1 0 +3 +1 |
| 3 | d9k | 25 | 0, 210, 900, 810 | Y | left | 8 4 4 6 | 7 4 7 7 | -1 0 +3 +1 |
| 4 | d9k | 40 | 280, 480, 1180, 1080 | Y | left | 6 4 4 6 | 5 4 6 7 | -1 0 +2 +1 |
| 5 | d9k | 53 | 690, 170, 1590, 770 | X | left | 8 4 4 6 | 7 4 7 7 | -1 0 +3 +1 |
| 6 | d9kt | 9 | 890, 480, 1790, 1080 | X | left | 7 4 4 6 | 6 4 7 7 | -1 0 +3 +1 |
| 7 | d9kt | 24 | 700, 480, 1600, 1080 | X | left | 7 4 4 6 | 6 4 7 7 | -1 0 +3 +1 |
| 8 | d9kt | 30 | 930, 360, 1830, 960 | Y | left | 8 4 5 6 | 7 4 7 7 | -1 0 +2 +1 |

| | old | new | mean delta | up / down |
|---|---|---|---|---|
| weight | 7.38 | 6.38 | **-1.00** | 0 / 8 |
| men (left trench) | 4.00 | 4.00 | **0.00** | 0 / 0 |
| tracers | 4.25 | 6.75 | +2.50 | 8 / 0 |
| smoke | 6.00 | 6.88 | +0.88 | 7 / 0 |

The critic found one systematic difference, and every time it was the new look:

> The one systematic difference is how tracers meet the smoke. One look draws the tracers on top of the smoke
> clouds as thick, even, saturated bars. That makes the fire look denser and slightly heavier, but the tracers read
> as flat painted-on stripes and they weaken the smoke's sense of volume. The other look hides or fogs the tracers
> inside the smoke, leaving thin bright cores, which reads much better as bullets flying through a 3D, smoke-filled
> space. Soldier readability, smoke shapes and explosions were identical in every pair.

The critic did not see the smokeSoft or burstGlow change: it called the smoke shapes identical. The smoke score
rose only because the tracers stopped cutting across the clouds.

## Verdict: reject the set as one default change

Rule 6 on the men passes: the mean delta is 0, and no pair is lower. The weight check fails. The mean weight delta
is -1.00, with 8 of 8 pairs lower, against a floor of -0.5.

Together, the tracer part costs more weight than it did alone: -1 in 8 of 8 here, against -0.33 in c104s (one
critic and one pair set, so part of the gap may be critic anchoring). The +0.17 weight from C99 does not offset it.
C99 alone (`fx.smokeSoft=0.6,fx.burstGlow=0.5`) still stands on its own pass in batch9-critic.md. C104 at shape 0
needs something that restores barrage weight before it becomes the default.
